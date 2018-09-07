;;;; core/request-response.lisp

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

(defpackage "CL-ANET-API/CORE/REQUEST-RESPONSE"
  (:use "CL")
  (:import-from "ALEXANDRIA")
  (:import-from "WU-DECIMAL")
  (:import-from "CL-JSON")
  (:import-from "PURI")
  (:import-from "DRAKMA")
  (:export "MAKE-MERCHANT-AUTHENTICATION"
           "MAKE-CREDIT-CARD"
           "MAKE-ORDER"
           "MAKE-LINE-ITEM"
           "MAKE-TAX"
           "MAKE-CUSTOMER"
           "MAKE-CUSTOMER-ADDRESS"
           ;;
           "TRANSACTION-REQUEST"
           "TRANSACTION-REQUEST-TYPE"
           "TRANSACTION-REQUEST-ALLOW-PARTIAL-AUTH-P"
           "TRANSACTION-REQUEST-DUPLICATE-WINDOW"
           "TRANSACTION-REQUEST-AMOUNT"
           "TRANSACTION-REQUEST-PAYMENT"
           "TRANSACTION-REQUEST-ORDER"
           "TRANSACTION-REQUEST-LINE-ITEMS"
           "TRANSACTION-REQUEST-TAX"
           "TRANSACTION-REQUEST-CUSTOMER"
           "TRANSACTION-REQUEST-BILL-TO"
           "TRANSACTION-REQUEST-CUSTOMER-IP"
           "TRANSACTION-REQUEST-SETTINGS"
           ;;
           "TRANSACTION-RESPONSE"
           "TRANSACTION-RESPONSE-RESPONSE-CODE"
           "TRANSACTION-RESPONSE-RESPONSE-CODE-DESCRIPTION"
           "TRANSACTION-RESPONSE-AUTH-CODE"
           "TRANSACTION-RESPONSE-AVS-RESULT-CODE"
           "TRANSACTION-RESPONSE-CVV-RESULT-CODE"
           "TRANSACTION-RESPONSE-CAVV-RESULT-CODE"
           "TRANSACTION-RESPONSE-TRANS-ID"
           "TRANSACTION-RESPONSE-REF-TRANS-ID"
           "TRANSACTION-RESPONSE-TRANS-HASH"
           "TRANSACTION-RESPONSE-ACCOUNT-NUMBER"
           "TRANSACTION-RESPONSE-ACCOUNT-TYPE"
           "TRANSACTION-RESPONSE-MESSAGE-CODE"
           "TRANSACTION-RESPONSE-MESSAGE-DESCRIPTION"
           "TRANSACTION-RESPONSE-ERROR-CODE"
           "TRANSACTION-RESPONSE-ERROR-TEXT"
           ;;
           "REQUEST"
           "REQUEST-MERCHANT-AUTHENTICATION"
           "REQUEST-REF-ID"
           "REQUEST-TRANSACTION-REQUEST"
           ;;
           "RESPONSE"
           "RESPONSE-REF-ID"
           "RESPONSE-RESULT-CODE"
           "RESPONSE-MESSAGE-CODE"
           "RESPONSE-MESSAGE-TEXT"
           "RESPONSE-TRANSACTION-RESPONSE"
           ;;
           "TRANSACTION-REQUEST-TRANSACTION-RESPONSE"
           ;;
           "API-ERROR"
           "EXECUTION-FAILED"
           "EXECUTION-FAILED-REQUEST"
           "EXECUTION-FAILED-ENDPOINT"
           "EXECUTION-FAILED-STATUS-CODE"
           "EXECUTION-FAILED-HEADERS"
           "TRANSACTION-FAILED"
           "TRANSACTION-FAILED-REQUEST"
           "TRANSACTION-FAILED-RAW-RESPONSE"
           "EXECUTE"))

(in-package "CL-ANET-API/CORE/REQUEST-RESPONSE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (wu-decimal:enable-reader-macro)
  (wu-decimal:enable-decimal-printing-for-ratios))

(defconstant +http-ok+ 200)

(defparameter *result-code-ok* "Ok")

;; See http://developer.authorize.net/api/reference/
(defparameter *default-customer-ip* "255.255.255.255")

(defstruct (merchant-authentication (:constructor make-merchant-authentication (api-login-id
                                                                                transaction-key)))
  (api-login-id nil :type string :read-only t)
  (transaction-key nil :type string :read-only t))

(defstruct (credit-card (:constructor make-credit-card (number expiration-date security-code)))
  (number nil :type string :read-only t)
  (expiration-date nil :type string :read-only t)
  (security-code nil :type string :read-only t))

(defstruct (order (:constructor make-order (invoice-number &optional (description ""))))
  (invoice-number nil :type string :read-only t)
  (description nil :type string :read-only t))

(defstruct (line-item (:constructor make-line-item (item-id name description quantity price)))
  (item-id nil :type string :read-only t) ; max. 31 chars
  (name nil :type string :read-only t) ; max. 31 chars
  (description nil :type string :read-only t) ; max. 255 chars
  (quantity 0 :type (integer 0) :read-only t) ; max. 2 decimal places, positive
  (price 0 :type (rational 0) :read-only t)) ; excluding tax, shipping, and duty

(defstruct (tax (:constructor make-tax (amount name description)))
  (amount nil :type (rational 0) :read-only t) ; max. 2 decimal places
  (name nil :type string :read-only t)
  (description nil :type string :read-only t))

(defstruct (customer (:constructor make-customer (type id email)))
  (type nil :type string :read-only t) ; "individual" or "business"
  (id nil :type string :read-only t) ; max. 20 alphanumeric chars
  (email nil :type string :read-only t)) ; max. 255 chars

(defstruct customer-address
  (first-name "" :type string :read-only t) ; max. 50 alphanumeric chars
  (last-name "" :type string :read-only t) ; max. 50 alphanumeric chars
  (company "" :type string :read-only t) ; max. 50 alphanumeric chars
  (address "" :type string :read-only t) ; max. 60 alphanumeric chars
  (city "" :type string :read-only t) ; max. 40 alphanumeric chars
  (state "" :type string :read-only t) ; max. 40 alphanumeric chars
  (zip "" :type string :read-only t) ; max. 20 alphanumeric chars
  (country "" :type string :read-only t) ; max. 60 alphanumeric chars
  (phone-number "" :type string :read-only t) ; max. 25 numeric, space, (, ), - chars
  (fax-number "" :type string :read-only t))  ; max. 25 numeric, space, (, ), - chars

(defclass transaction-request () ())

(defgeneric transaction-request-type (transaction-request))

(defgeneric transaction-request-allow-partial-auth-p (transaction-request))

(defgeneric (setf transaction-request-allow-partial-auth-p) (value transaction-request))

(defgeneric transaction-request-duplicate-window (transaction-request))

(defgeneric (setf transaction-request-duplicate-window) (value transaction-request))

(defgeneric transaction-request-amount (transaction-request))

(defgeneric transaction-request-payment (transaction-request))

(defgeneric transaction-request-order (transaction-request))

(defgeneric transaction-request-line-items (transaction-request))

(defgeneric transaction-request-tax (transaction-request))

(defgeneric transaction-request-customer (transaction-request))

(defgeneric transaction-request-bill-to (transaction-request))

(defgeneric transaction-request-customer-ip (transaction-request))

(defgeneric transaction-request-settings (transaction-request))

(defclass transaction-response () ())

(defgeneric transaction-response-response-code (transaction-response))

(defparameter *transaction-response-response-code-approved* "1")
(defparameter *transaction-response-response-code-declined* "2")
(defparameter *transaction-response-response-code-error* "3")
(defparameter *transaction-response-response-code-held-for-review* "4")

(defun transaction-response-response-code-description (transaction-response)
  (let ((response-code (transaction-response-response-code transaction-response)))
    (cond ((string= *transaction-response-response-code-approved* response-code)
           :approved)
          ((string= *transaction-response-response-code-declined* response-code)
           :declined)
          ((string= *transaction-response-response-code-error* response-code)
           :error)
          ((string= *transaction-response-response-code-held-for-review* response-code)
           :held-for-review))))

(defgeneric transaction-response-auth-code (transaction-response))

(defgeneric transaction-response-avs-result-code (transaction-response))

(defgeneric transaction-response-cvv-result-code (transaction-response))

(defgeneric transaction-response-cavv-result-code (transaction-response))

(defgeneric transaction-response-trans-id (transaction-response))

(defgeneric transaction-response-ref-trans-id (transaction-response))

(defgeneric transaction-response-trans-hash (transaction-response))

(defgeneric transaction-response-account-number (transaction-response))

(defgeneric transaction-response-account-type (transaction-response))

(defgeneric transaction-response-message-code (transaction-response))

(defgeneric transaction-response-message-description (transaction-response))

(defgeneric transaction-response-error-code (transaction-response))

(defgeneric transaction-response-error-text (transaction-response))

(defclass request ()
  ((merchant-authentication :initarg :merchant-authentication :reader request-merchant-authentication)
   (ref-id :initarg :ref-id :reader request-ref-id)
   (transaction-request :initarg :transaction-request :reader request-transaction-request))
  (:default-initargs
   :merchant-authentication nil
    :ref-id ""
    :transaction-request nil))

(defclass response ()
  ((ref-id :initarg :ref-id :accessor response-ref-id)
   (result-code :initarg :result-code :accessor response-result-code)
   (message-code :initarg :message-code :accessor response-message-code)
   (message-text :initarg :message-text :accessor response-message-text)
   (transaction-response :initarg :transaction-response :accessor response-transaction-response))
  (:default-initargs
   :ref-id ""
    :result-code ""
    :message-code ""
    :message-text ""
    :transaction-response nil))

(defmethod cl-json:encode-json ((object merchant-authentication) &optional (stream cl-json:*json-output*))
  (with-accessors ((merchant-authentication-api-login-id merchant-authentication-api-login-id)
                   (merchant-authentication-transaction-key merchant-authentication-transaction-key))
      object
    (cl-json:with-object (stream)
      (cl-json:encode-object-member :name merchant-authentication-api-login-id stream)
      (cl-json:encode-object-member :transaction-key merchant-authentication-transaction-key stream))))

(defmethod cl-json:encode-json ((object credit-card) &optional (stream cl-json:*json-output*))
  (with-accessors ((credit-card-number credit-card-number)
                   (credit-card-expiration-date credit-card-expiration-date)
                   (credit-card-security-code credit-card-security-code))
      object
    (cl-json:with-object (stream)
      (cl-json:as-object-member (:credit-card stream)
        (cl-json:with-object (stream)
          (cl-json:encode-object-member :card-number credit-card-number stream)
          (cl-json:encode-object-member :expiration-date credit-card-expiration-date stream)
          (cl-json:encode-object-member :card-code credit-card-security-code stream))))))

(defmethod cl-json:encode-json ((object order) &optional (stream cl-json:*json-output*))
  (with-accessors ((order-invoice-number order-invoice-number)
                   (order-description order-description))
      object
    (cl-json:with-object (stream)
      (cl-json:encode-object-member :invoice-number order-invoice-number stream)
      (cl-json:encode-object-member :description order-description stream))))

(defmethod cl-json:encode-json ((object line-item) &optional (stream cl-json:*json-output*))
  (with-accessors ((line-item-item-id line-item-item-id)
                   (line-item-name line-item-name)
                   (line-item-description line-item-description)
                   (line-item-quantity line-item-quantity)
                   (line-item-price line-item-price))
      object
    (cl-json:with-object (stream)
      (cl-json:encode-object-member :item-id line-item-item-id stream)
      (cl-json:encode-object-member :name line-item-name stream)
      (cl-json:encode-object-member :description line-item-description stream)
      (cl-json:encode-object-member :quantity (princ-to-string line-item-quantity) stream)
      (cl-json:encode-object-member :unit-price (let ((wu-decimal:*print-precision-loss* :round))
                                                  (format nil "~/wu-decimal:$/" line-item-price))
                                    stream))))

(defmethod cl-json:encode-json ((object tax) &optional (stream cl-json:*json-output*))
  (with-accessors ((tax-amount tax-amount)
                   (tax-name tax-name)
                   (tax-description tax-description))
      object
    (cl-json:with-object (stream)
      (cl-json:encode-object-member :amount (let ((wu-decimal:*print-precision-loss* :round))
                                              (format nil "~/wu-decimal:$/" tax-amount))
                                    stream)
      (cl-json:encode-object-member :name tax-name stream)
      (cl-json:encode-object-member :description tax-description stream))))

(defmethod cl-json:encode-json ((object customer) &optional (stream cl-json:*json-output*))
  (with-accessors ((customer-type customer-type)
                   (customer-id customer-id)
                   (customer-email customer-email))
      object
    (cl-json:with-object (stream)
      (cl-json:encode-object-member :type customer-type stream)
      (cl-json:encode-object-member :id customer-id stream)
      (cl-json:encode-object-member :email customer-email stream))))

(defmethod cl-json:encode-json ((object customer-address) &optional (stream cl-json:*json-output*))
  (with-accessors ((customer-address-first-name customer-address-first-name)
                   (customer-address-last-name customer-address-last-name)
                   (customer-address-company customer-address-company)
                   (customer-address-address customer-address-address)
                   (customer-address-city customer-address-city)
                   (customer-address-state customer-address-state)
                   (customer-address-zip customer-address-zip)
                   (customer-address-country customer-address-country)
                   (customer-address-phone-number customer-address-phone-number)
                   (customer-address-fax-number customer-address-fax-number))
      object
    (cl-json:with-object (stream)
      (cl-json:encode-object-member :first-name customer-address-first-name stream)
      (cl-json:encode-object-member :last-name customer-address-last-name stream)
      (cl-json:encode-object-member :company customer-address-company stream)
      (cl-json:encode-object-member :address customer-address-address stream)
      (cl-json:encode-object-member :city customer-address-city stream)
      (cl-json:encode-object-member :state customer-address-state stream)
      (cl-json:encode-object-member :zip customer-address-zip stream)
      (cl-json:encode-object-member :country customer-address-country stream)
      (cl-json:encode-object-member :phone-number customer-address-phone-number stream)
      (cl-json:encode-object-member :fax-number customer-address-fax-number stream))))

(defmethod cl-json:encode-json ((request request) &optional (stream cl-json:*json-output*))
  (with-accessors ((request-merchant-authentication request-merchant-authentication)
                   (request-ref-id request-ref-id)
                   (request-transaction-request request-transaction-request))
      request
    (cl-json:with-object (stream)
      (cl-json:as-object-member (:create-transaction-request stream)
        (cl-json:with-object (stream)
          (cl-json:encode-object-member :merchant-authentication request-merchant-authentication stream)
          (cl-json:encode-object-member :ref-id request-ref-id stream)
          (cl-json:encode-object-member :transaction-request request-transaction-request stream))))))

(define-condition api-error (error)
  ()
  (:documentation "Superclass for all errors signaled by the CL-ANET-API API."))

(define-condition execution-failed (api-error)
  ((request :initarg :request :reader execution-failed-request)
   (endpoint :initarg :endpoint :reader execution-failed-endpoint)
   (status-code :initarg :status-code :reader execution-failed-status-code)
   (headers :initarg :headers :reader execution-failed-headers))
  (:report (lambda (condition stream)
	     (format stream
                     "Execution failed: request = ~A endpoint = ~A status-code = ~A headers = ~A."
                     (execution-failed-request condition)
                     (execution-failed-endpoint condition)
                     (execution-failed-status-code condition)
                     (execution-failed-headers condition))))
  (:documentation "Signalled by EXCUTE when either the API request fails or the response is invalid."))

(define-condition transaction-failed (api-error)
  ((request :initarg :request :reader transaction-failed-request)
   (raw-response :initarg :raw-response :reader transaction-failed-raw-response))
  (:report (lambda (condition stream)
	     (format stream
                     "Transaction failed: request = ~A raw-response = ~A."
                     (transaction-failed-request condition)
                     (transaction-failed-raw-response condition))))
  (:documentation "Signalled by HTTP-GET when the requested transaction fails."))

(defun application-json-content-type-p (content-type)
  (alexandria:starts-with-subseq "application/json" content-type :test #'string-equal))

;; Trim the leading BOM UTF-8 sequence. See https://en.wikipedia.org/wiki/Byte_order_mark.
(defun trim-bom (octets)
  (subseq octets 3))

(defun parse-raw-response (octets)
  (let ((string (flexi-streams:octets-to-string (trim-bom octets)))
        (cl-json:*json-symbols-package* :keyword))
    (cl-json:decode-json-from-string string)))

(defun raw-response-result-code (raw-response)
  (cdr (assoc :result-code (cdr (assoc :messages raw-response)))))

(defun raw-response-ok-p (raw-response)
  (string= *result-code-ok* (raw-response-result-code raw-response)))

(defun raw-response-ref-id (raw-response)
  (cdr (assoc :ref-id raw-response)))

(defun raw-response-message-code (raw-response)
  (cdr (assoc :code (cadr (assoc :message (cdr (assoc :messages raw-response)))))))

(defun raw-response-message-text (raw-response)
  (cdr (assoc :text (cadr (assoc :message (cdr (assoc :messages raw-response)))))))

(defun raw-response-transaction-response (raw-response)
  (cdr (assoc :transaction-response raw-response)))

(defun raw-response-transaction-response-message (raw-response)
  (cadr (assoc :messages (raw-response-transaction-response raw-response))))

(defun raw-response-transaction-response-message-code (raw-response)
  (cdr (assoc :code (raw-response-transaction-response-message raw-response))))

(defun raw-response-transaction-response-message-text (raw-response)
  (cdr (assoc :text (raw-response-transaction-response-message raw-response))))

(defgeneric transaction-request-transaction-response
    (transaction-request raw-response-transaction-response))

(defun execute (request endpoint)
  (check-type request request)
  (check-type endpoint puri:uri)
  (let ((request-json (cl-json:encode-json-to-string request)))
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (drakma:http-request endpoint
                             :method :post
                             :force-ssl t
                             :accept "application/json"
                             :content-type "application/json"
                             :content request-json
                             :external-format-out :utf-8
                             :external-format-in :utf-8)
      (declare (ignore uri stream must-close reason-phrase))
      (unless (and (= +http-ok+ status-code)
                   (application-json-content-type-p (drakma:header-value :content-type headers)))
        (error 'execution-failed
               :request request
               :endpoint endpoint
               :status-code status-code
               :headers headers))
      (let ((raw-response (parse-raw-response body)))
        (unless (raw-response-ok-p raw-response)
          (error 'transaction-failed :request request :raw-response raw-response))
        (when (null (raw-response-transaction-response raw-response))
          (error 'transaction-failed :request request :raw-response raw-response))
        (make-instance 'response
                       :ref-id (raw-response-ref-id raw-response)
                       :result-code (raw-response-result-code raw-response)
                       :message-code (raw-response-message-code raw-response)
                       :message-text (raw-response-message-text raw-response)
                       :transaction-response (transaction-request-transaction-response
                                              (request-transaction-request request)
                                              (raw-response-transaction-response raw-response)))))))
