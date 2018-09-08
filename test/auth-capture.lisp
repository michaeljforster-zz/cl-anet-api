;;;; test/auth-capture.lisp

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

(defpackage "CL-ANET-API/TEST/AUTH-CAPTURE"
  (:use "CL"
        "LISP-UNIT")
  (:import-from "CL-ANET-API/TEST/SETUP")
  (:import-from "CL-ANET-API"))

(in-package "CL-ANET-API/TEST/AUTH-CAPTURE")

(defun execute-transaction (ref-id customer-data order-data card-data card-expiration-date zip-code)
  (let* ((merchant-authentication
          (cl-anet-api:make-merchant-authentication cl-anet-api/test/setup:*api-login-id*
                                                    cl-anet-api/test/setup:*transaction-key*))
         (credit-card (cl-anet-api:make-credit-card (getf card-data :card-number)
                                                    card-expiration-date
                                                    (getf card-data :card-security-code)))
         (order (cl-anet-api:make-order (getf order-data :invoice-number)
                                        (getf order-data :description)))
         (line-items (mapcar #'(lambda (item)
                                 (cl-anet-api:make-line-item (getf item :item-id)
                                                             (getf item :name)
                                                             (getf item :description)
                                                             (getf item :quantity)
                                                             (getf item :unit-price)
                                                             (getf item :taxable)))
                             (getf order-data :items)))
         (tax (cl-anet-api:make-tax (getf order-data :tax-amount)
                                    (getf order-data :tax-name)
                                    (getf order-data :tax-name)))
         (customer (cl-anet-api:make-customer "individual"
                                              (getf customer-data :id)
                                              (getf customer-data :email-address)))
         (bill-to (cl-anet-api:make-customer-address :first-name (getf customer-data :first-name)
                                                     :last-name (getf customer-data :last-name)
                                                     :company (getf customer-data :company)
                                                     :address (getf customer-data :address)
                                                     :city (getf customer-data :city)
                                                     :state (getf customer-data :state)
                                                     :zip zip-code
                                                     :country (getf customer-data :country)
                                                     :phone-number (getf customer-data :phone-number)
                                                     :fax-number (getf customer-data :fax-number)))
         (transaction-request (make-instance 'cl-anet-api:auth-capture-transaction-request
                                             :amount (getf order-data :amount)
                                             :payment credit-card
                                             :order order
                                             :line-items line-items
                                             :tax tax
                                             :customer customer
                                             :bill-to bill-to
                                             :customer-ip (getf customer-data :ip-address)
                                             ;; :settings
                                             )))
    (setf (cl-anet-api:transaction-request-duplicate-window transaction-request)
          cl-anet-api/test/setup:*duplicate-window*)
    (let ((request (make-instance 'cl-anet-api:request
                                  :merchant-authentication merchant-authentication
                                  :ref-id ref-id
                                  :transaction-request transaction-request)))
      (let ((response (cl-anet-api:execute request cl-anet-api/test/setup:*endpoint*)))
        (values response (cl-anet-api:response-transaction-response response))))))

(define-test auth-capture-approved
  (multiple-value-bind (response transaction-response)
      (execute-transaction "REF-0001"
                           cl-anet-api/test/setup:*customer*
                           cl-anet-api/test/setup:*order*
                           (cl-anet-api/test/setup:find-card "4007000000027")
                           (cl-anet-api/test/setup:good-card-expiration-date)
                           cl-anet-api/test/setup:*good-zip-code*)
    (assert-equal "REF-0001" (cl-anet-api:response-ref-id response))
    (assert-equal "Ok" (cl-anet-api:response-result-code response))
    (assert-equal "I00001" (cl-anet-api:response-message-code response))
    (assert-equal "Successful." (cl-anet-api:response-message-text response))
    (assert-equal "1" (cl-anet-api:transaction-response-response-code transaction-response))
    (assert-equal :approved (cl-anet-api:transaction-response-response-code-description transaction-response))
    ;; not empty (assert-equal "" (cl-anet-api:transaction-response-auth-code transaction-response))
    (assert-equal "1" (cl-anet-api:transaction-response-message-code transaction-response))
    (assert-equal "This transaction has been approved." (cl-anet-api:transaction-response-message-description transaction-response))
    (assert-equal "" (cl-anet-api:transaction-response-error-code transaction-response))
    (assert-equal "" (cl-anet-api:transaction-response-error-text transaction-response))))

(define-test auth-capture-declined-expired
  (assert-error 'cl-anet-api:transaction-failed
                (execute-transaction "REF-0001"
                                     cl-anet-api/test/setup:*customer*
                                     cl-anet-api/test/setup:*order*
                                     (cl-anet-api/test/setup:find-card "4007000000027")
                                     (cl-anet-api/test/setup:bad-card-expiration-date)
                                     cl-anet-api/test/setup:*good-zip-code*)))

;;; TODO more cases
