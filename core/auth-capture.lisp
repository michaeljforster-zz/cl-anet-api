;;;; core/auth-capture.lisp

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

(defpackage "CL-ANET-API/CORE/AUTH-CAPTURE"
  (:use "CL")
  (:import-from "ALEXANDRIA")
  (:import-from "WU-DECIMAL")
  (:import-from "CL-JSON")
  (:import-from "CL-ANET-API/CORE/REQUEST-RESPONSE")
  (:export "AUTH-CAPTURE-TRANSACTION-REQUEST"
           "AUTH-CAPTURE-TRANSACTION-RESPONSE"))

(in-package "CL-ANET-API/CORE/AUTH-CAPTURE")

;; See http://developer.authorize.net/api/reference/
(defparameter *default-customer-ip* "255.255.255.255")

(defclass auth-capture-transaction-request (cl-anet-api/core/request-response:transaction-request)
  ((amount :initarg :amount :reader cl-anet-api/core/request-response:transaction-request-amount) ; total, including tax, shipping, and other charges; 15 digits
   (payment :initarg :payment :reader cl-anet-api/core/request-response:transaction-request-payment)
   (order :initarg :order :reader cl-anet-api/core/request-response:transaction-request-order)
   (line-items :initarg :line-items :reader cl-anet-api/core/request-response:transaction-request-line-items)
   (tax :initarg :tax :reader cl-anet-api/core/request-response:transaction-request-tax)
   (customer :initarg :customer :reader cl-anet-api/core/request-response:transaction-request-customer)
   (bill-to :initarg :bill-to :reader cl-anet-api/core/request-response:transaction-request-bill-to)
   (customer-ip :initarg :customer-ip :reader cl-anet-api/core/request-response:transaction-request-customer-ip)
   (settings :initform '() :reader cl-anet-api/core/request-response:transaction-request-settings))
  (:default-initargs
   :amount #$0.00
    :payment nil
    :order nil
    :line-items '()
    :tax nil
    :customer nil
    :bill-to nil
    :customer-ip *default-customer-ip*))

(defmethod cl-anet-api/core/request-response:transaction-request-type ((transaction-request auth-capture-transaction-request))
  "authCaptureTransaction")

(defmethod cl-anet-api/core/request-response:transaction-request-allow-partial-auth-p ((transaction-request auth-capture-transaction-request))
  (getf (slot-value transaction-request 'settings) :allow-partial-auth))

(defmethod (setf cl-anet-api/core/request-response:transaction-request-allow-partial-auth-p) (value (transaction-request auth-capture-transaction-request))
  (check-type value boolean)
  (setf (getf (slot-value transaction-request 'settings) :allow-partial-auth) value))

(defmethod cl-anet-api/core/request-response:transaction-request-duplicate-window ((transaction-request auth-capture-transaction-request))
  (getf (slot-value transaction-request 'settings) :duplicate-window))

(defmethod (setf cl-anet-api/core/request-response:transaction-request-duplicate-window) (value (transaction-request auth-capture-transaction-request))
  (check-type value integer)
  (setf (getf (slot-value transaction-request 'settings) :duplicate-window) value))

(defclass auth-capture-transaction-response (cl-anet-api/core/request-response:transaction-response)
  ((response-code :initarg :response-code :reader cl-anet-api/core/request-response:transaction-response-response-code)
   (auth-code :initarg :auth-code :reader cl-anet-api/core/request-response:transaction-response-auth-code)
   (avs-result-code :initarg :avs-result-code :reader cl-anet-api/core/request-response:transaction-response-avs-result-code)
   (cvv-result-code :initarg :cvv-result-code :reader cl-anet-api/core/request-response:transaction-response-cvv-result-code)
   (cavv-result-code :initarg :cavv-result-code :reader cl-anet-api/core/request-response:transaction-response-cavv-result-code)
   (trans-id :initarg :trans-id :reader cl-anet-api/core/request-response:transaction-response-trans-id)
   ;; NOTE watch JSON decode of "refTransID" as :REF-TRANS-+ID+
   (ref-trans-id :initarg :ref-trans-id :reader cl-anet-api/core/request-response:transaction-response-ref-trans-id)
   (trans-hash :initarg :trans-hash :reader cl-anet-api/core/request-response:transaction-response-trans-hash)
   (account-number :initarg :account-number :reader cl-anet-api/core/request-response:transaction-response-account-number)
   (account-type :initarg :account-type :reader cl-anet-api/core/request-response:transaction-response-account-type)
   (message-code :initarg :message-code :reader cl-anet-api/core/request-response:transaction-response-message-code)
   (message-description :initarg :message-description :reader cl-anet-api/core/request-response:transaction-response-message-description)
   (error-code :initarg :error-code :reader cl-anet-api/core/request-response:transaction-response-error-code)
   (error-text :initarg :error-text :reader cl-anet-api/core/request-response:transaction-response-error-text)
   ;; LATER other fields...
   )
  (:default-initargs
   :response-code ""
    :auth-code ""
    :avs-result-code ""
    :cvv-result-code ""
    :cavv-result-code ""
    :trans-id ""
    :ref-trans-id ""
    :trans-hash ""
    :account-number ""
    :account-type ""
    :message-code ""
    :message-description ""
    :error-code ""
    :error-text ""))

(defmethod cl-json:encode-json ((transaction-request auth-capture-transaction-request)
                                &optional (stream cl-json:*json-output*))
  (with-accessors ((transaction-request-type cl-anet-api/core/request-response:transaction-request-type)
                   (transaction-request-amount cl-anet-api/core/request-response:transaction-request-amount)
                   (transaction-request-payment cl-anet-api/core/request-response:transaction-request-payment)
                   (transaction-request-order cl-anet-api/core/request-response:transaction-request-order)
                   (transaction-request-line-items cl-anet-api/core/request-response:transaction-request-line-items)
                   (transaction-request-tax cl-anet-api/core/request-response:transaction-request-tax)
                   (transaction-request-customer cl-anet-api/core/request-response:transaction-request-customer)
                   (transaction-request-bill-to cl-anet-api/core/request-response:transaction-request-bill-to)
                   (transaction-request-customer-ip cl-anet-api/core/request-response:transaction-request-customer-ip)
                   (transaction-request-settings cl-anet-api/core/request-response:transaction-request-settings))
      transaction-request
    (cl-json:with-object (stream)
      (cl-json:encode-object-member :transaction-type transaction-request-type stream)
      (cl-json:encode-object-member :amount (let ((wu-decimal:*print-precision-loss* :round))
                                              (format nil "~/wu-decimal:$/" transaction-request-amount))
                                    stream)
      (unless (null transaction-request-payment)
        (cl-json:encode-object-member :payment transaction-request-payment stream))
      (unless (null transaction-request-order)
        (cl-json:encode-object-member :order transaction-request-order stream))
      (unless (null transaction-request-line-items)
        (cl-json:as-object-member (:line-items stream)
          (cl-json:with-object (stream)
            (dolist (line-item transaction-request-line-items)
              (cl-json:encode-object-member :line-item line-item stream)))))
      (unless (null transaction-request-tax)
        (cl-json:encode-object-member :tax transaction-request-tax stream))
      (unless (null transaction-request-customer)
        (cl-json:encode-object-member :customer transaction-request-customer stream))
      (unless (null transaction-request-bill-to)
        (cl-json:encode-object-member :bill-to transaction-request-bill-to stream))
      (cl-json:encode-object-member :customer-i-p transaction-request-customer-ip stream)
      (unless (null transaction-request-settings) ; assuming a list implementation
        (cl-json:as-object-member (:transaction-settings stream)
          (cl-json:with-object (stream)
            (alexandria:doplist (key value transaction-request-settings)
              (ecase key
                (:allow-partial-auth
                 (cl-json:as-object-member (:setting stream)
                   (cl-json:encode-object-member :setting-name key stream)
                   (cl-json:encode-object-member :setting-value (if value "True" "False")
                                                 stream)))
                (:duplicate-window
                 (cl-json:as-object-member (:setting stream)
                   (cl-json:encode-object-member :setting-name key stream)
                   (cl-json:encode-object-member :setting-value (princ-to-string value)
                                                 stream)))))))))))

(defmethod cl-anet-api/core/request-response:transaction-request-transaction-response
    ((transaction-request auth-capture-transaction-request)
     raw-response-transaction-response)
  (flet ((value (key)
           (cdr (assoc key raw-response-transaction-response))))
    (let ((message (cadr (assoc :messages raw-response-transaction-response)))
          (err (cadr (assoc :errors raw-response-transaction-response))))
      (make-instance 'auth-capture-transaction-response
                     :response-code (value :response-code)
                     :auth-code (value :auth-code)
                     :avs-result-code (value :avs-result-code)
                     :cvv-result-code (value :cvv-result-code)
                     :cavv-result-code (value :cavv-result-code)
                     :trans-id (value :trans-id)
                     :ref-trans-id (value :ref-trans-+id+) ; Parsed from "refTransID"!
                     :trans-hash (value :trans-hash)
                     :account-number (value :account-number)
                     :account-type (value :account-type)
                     :message-code (or (cdr (assoc :code message)) "")
                     :message-description (or (cdr (assoc :description message)))
                     :error-code (or (cdr (assoc :error-code err)) "")
                     :error-text (or (cdr (assoc :error-text err)) "")))))
