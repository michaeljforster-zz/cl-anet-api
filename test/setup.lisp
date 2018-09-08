;;;; test/setup.lisp

;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2017 Michael J. Forster
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(defpackage "CL-ANET-API/TEST/SETUP"
  (:use "CL")
  (:export "*ENDPOINT*"
           "*API-LOGIN-ID*"
           "*TRANSACTION-KEY*"
           "*DUPLICATE-WINDOW*"
           "FIND-CARD"
           "GOOD-CARD-EXPIRATION-DATE"
           "BAD-CARD-EXPIRATION-DATE"
           "*GOOD-ZIP-CODE*"
           "*GENERAL-RESPONSES*"
           "*CVV-RESPONSES*"
           "*AVS-RESPONSES*"
           "*REF-ID*"
           "*CUSTOMER*"
           "*ORDER*"))

(in-package "CL-ANET-API/TEST/SETUP")

(defparameter *endpoint* #u"https://apitest.authorize.net/xml/v1/request.api"
  "The Authorize.Net API Endpoint. The initial value is the URL of the
Authorize.Net Sandbox API Endpoint. See
https://developer.authorize.net/api/reference/index.html")

(defparameter *api-login-id* nil
  "The Authorize.Net API Login ID. The initial value is NIL. See
https://developer.authorize.net/api/reference/index.html")

(defparameter *transaction-key* nil
  "The Authorize.Net API Transaction Key. The initial value is
NIL. See https://developer.authorize.net/api/reference/index.html")

(defparameter *duplicate-window* 0)

(defparameter *cards*
  '((:card-brand "American Express" :card-number "370000000000002" :card-security-code "1234")
    (:card-brand "Discover" :card-number "6011000000000012" :card-security-code "123")
    (:card-brand "JCB" :card-number "3088000000000017" :card-security-code "123")
    (:card-brand "Diners Club/Carte Blanche" :card-number "38000000000006" :card-security-code "123")
    (:card-brand "Visa" :card-number "4007000000027" :card-security-code "123")
    (:card-brand "Visa" :card-number "4012888818888" :card-security-code "123")
    (:card-brand "Visa" :card-number "4111111111111111" :card-security-code "123")
    (:card-brand "Mastercard" :card-number "5424000000000015" :card-security-code "123")
    (:card-brand "Mastercard" :card-number "2223000010309703" :card-security-code "123")
    (:card-brand "Mastercard" :card-number "2223000010309711" :card-security-code "123"))
  "The Authorize.Net test card numbers. See
https://developer.authorize.net/hello_world/testing_guide/")

(defun find-card (card-number)
  (find card-number
        *cards*
        :key #'(lambda (plist) (getf plist :card-number))
        :test #'string=))

(defun good-card-expiration-date ()
  "Return an expiration date after today’s date. See
https://developer.authorize.net/hello_world/testing_guide/"
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour date day daylight-p zone))
    (format nil "~4,D-~2,'0D" year (1+ month))))

(defun bad-card-expiration-date ()
  "Return an expiration date before today’s date. See
https://developer.authorize.net/hello_world/testing_guide/"
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second minute hour date day daylight-p zone))
    (format nil "~4,D-~2,'0D" year (1- month))))

(defparameter *good-zip-code* "44628")

(defparameter *general-responses*
  '((:zip-code "46282" :response-code "2" :response-text "This Transaction has been declined."))
  "The Authorize.Net General bank decline test ZIP code and response. See
https://developer.authorize.net/hello_world/testing_guide/")

(defparameter *avs-responses*
  '((:zip-code "46203" :avs-status "E" :avs-response "AVS data provided is invalid or AVS is not allowed for the card type that was used.")
    (:zip-code "46207" :avs-status "R" :avs-response "The AVS system was unavailable at the time of processing.")
    ;; Not applicable to American Express
    (:zip-code "46204" :avs-status "G" :avs-response "The card issuing bank is of non-U.S. origin and does not support AVS.")
    (:zip-code "46209" :avs-status "U" :avs-response "The address information for the cardholder is unavailable.")
    (:zip-code "46208" :avs-status "S" :avs-response "The U.S. card issuing bank does not support AVS.")
    (:zip-code "46205" :avs-status "N" :avs-response "Address: No Match ZIP Code: No Match")
    (:zip-code "46201" :avs-status "A" :avs-response "Address: Match ZIP Code: No Match")
    (:zip-code "46217" :avs-status "Z" :avs-response "Address: No Match ZIP Code: Match")
    ;; Not applicable to American Express
    (:zip-code "46211" :avs-status "W" :avs-response "Address: No Match ZIP Code: Matched 9 digits")
    ;; Not applicable to Visa or American Express
    (:zip-code "46214" :avs-status "X" :avs-response "Address: Match ZIP Code: Matched 9 digits"))
  "The Authorize.Net AVS test ZIP codes and responses. See
https://developer.authorize.net/hello_world/testing_guide/")

(defparameter *cvv-responses*
  '((:card-code "900" :cvv-response "M" :cvv-status "Successful Match")
    (:card-code "901" :cvv-response "N" :cvv-status "Does NOT Match")
    (:card-code "904" :cvv-response "P" :cvv-status "Is NOT Processed")
    (:card-code "902" :cvv-response "S" :cvv-status "Should be on card, but is not indicated")
    (:card-code "903" :cvv-response "U" :cvv-status "Issuer is not certified or has not provided encryption key"))
  "The Authorize.Net CVV test card codes and responses. See
https://developer.authorize.net/hello_world/testing_guide/")

(defparameter *customer*
  '(:email-address "ellen.johnson@souveniropolis.com"
    :id "0123456789ABCDEF"
    :first-name "Ellen"
    :last-name "Johnson"
    :company "Souveniropolis"
    :address "14 Main Street"
    :city "Pecan Springs"
    :state "TX"
    :country "USA"
    :phone-number "1-222-333-4444"
    :fax-number "1-222-333-5555"
    :ip-address "192.168.1.123"))

(defparameter *order*
  '(:invoice-number "INV-0123456789"
    :description "Customer Order"
    :items ((:item-id "W-0001"
             :name "Widget"
             :description "A widget"
             :quantity 1
             :unit-price 1999/100
             :taxable t)
            (:item-id "G-0001"
             :name "Gadget"
             :description "A gadget"
             :quantity 2
             :unit-price 1500/100
             :taxable t)
            (:item-id "F-0001"
             :name "Transaction Fee"
             :description ""
             :quantity 1
             :unit-price 199/100
             :taxable nil))
    :tax-name "Tax"
    :tax-amount 64987/10000
    :amount 584787/10000))
