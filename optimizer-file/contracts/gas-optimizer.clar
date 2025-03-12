;; Gas Fee Optimization Algorithm for Stacks
;; A tool to analyze and optimize transaction fees on the Stacks blockchain

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-params u101)  ;; Changed to uint instead of response
(define-constant err-contract-paused u102)  ;; Changed to uint instead of response
(define-constant err-unauthorized u103)  ;; Changed to uint instead of response
(define-constant err-invalid-nonce u104)  ;; Changed to uint instead of response

;; Status and security variables
(define-data-var contract-status (string-ascii 10) "active") ;; "active" or "paused"
(define-map authorized-admins principal bool)
(define-map user-nonces principal uint)

;; Data variables
(define-data-var min-recommended-fee uint u0)
(define-data-var max-recommended-fee uint u0)
(define-data-var last-update-height uint u0)

;; Maps
(define-map historical-fees uint uint)  ;; block-height -> median-fee
(define-map transaction-types (string-ascii 20) uint)  ;; transaction-type -> base-cost
(define-map optimal-time-windows 
  {tx-type: (string-ascii 20), priority: (string-ascii 10)}
  {start-hour: uint, end-hour: uint, day-preference: uint})

;; Security functions
(define-private (is-admin)
  (or (is-eq tx-sender contract-owner) (default-to false (map-get? authorized-admins tx-sender))))

(define-private (check-contract-active)
  (is-eq (var-get contract-status) "active"))

(define-private (check-and-update-nonce (user principal) (nonce uint))
  (let ((current-nonce (default-to u0 (map-get? user-nonces user))))
    (and (> nonce current-nonce)
         (begin
           (map-set user-nonces user nonce)
           true))))

;; Admin security functions
(define-public (set-contract-status (new-status (string-ascii 10)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (or (is-eq new-status "active") (is-eq new-status "paused")) (err err-invalid-params))
    (var-set contract-status new-status)
    (ok true)))

(define-public (add-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-admins admin true)
    (ok true)))

(define-public (remove-admin (admin principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete authorized-admins admin)
    (ok true)))

;; Public functions
(define-public (get-recommended-fee (transaction-type (string-ascii 20)))
  (begin
    (asserts! (check-contract-active) (err err-contract-paused))
    
    (let ((base-cost (default-to u0 (map-get? transaction-types transaction-type)))
          (current-height burn-block-height))
      (if (> base-cost u0)
          (ok (+ base-cost (calculate-dynamic-fee current-height)))
          (err err-invalid-params)))))

(define-public (suggest-optimal-time (transaction-type (string-ascii 20)) (priority (string-ascii 10)))
  (begin
    (asserts! (check-contract-active) (err err-contract-paused))
    (asserts! (or (is-eq priority "low") (is-eq priority "medium") (is-eq priority "high")) (err err-invalid-params))
    
    (let ((window (default-to 
                    {start-hour: u0, end-hour: u23, day-preference: u7} 
                    (map-get? optimal-time-windows {tx-type: transaction-type, priority: priority}))))
      
      ;; Call the fee predictor to get predictions for different times
      (ok {
        suggested-window: window,
        estimated-savings: (calculate-savings window)
      }))))

(define-private (calculate-savings (window {start-hour: uint, end-hour: uint, day-preference: uint}))
  ;; Simplified implementation for Phase 2
  ;; In future phases, will integrate more complex calculations
  (let ((avg-fee u1000)
        (window-discount (/ (* avg-fee u1500) u10000)))
    (- avg-fee window-discount)))

(define-public (set-optimal-window (tx-type (string-ascii 20)) 
                                   (priority (string-ascii 10))
                                   (start-hour uint)
                                   (end-hour uint)
                                   (day-preference uint))
  (begin
    (asserts! (is-admin) (err err-unauthorized))
    (asserts! (check-contract-active) (err err-contract-paused))
    (asserts! (< start-hour u24) (err err-invalid-params))
    (asserts! (< end-hour u24) (err err-invalid-params))
    (asserts! (>= end-hour start-hour) (err err-invalid-params))
    (asserts! (< day-preference u8) (err err-invalid-params))
    
    (map-set optimal-time-windows 
      {tx-type: tx-type, priority: priority} 
      {start-hour: start-hour, end-hour: end-hour, day-preference: day-preference})
    (ok true)))

;; Private functions
(define-private (calculate-dynamic-fee (current-height uint))
  (let ((result (fold calculate-avg-fee
                   (list u1 u2 u3 u4 u5)
                   {total: u0, count: u0})))
    ;; If no historical data found, use default fee
    (if (is-eq (get count result) u0)
      u1000
      (/ (get total result) (get count result)))))

(define-private (calculate-avg-fee (offset uint) (acc {total: uint, count: uint}))
  (let ((height (if (> burn-block-height offset) 
                   (- burn-block-height offset)
                   u0))
        (fee (default-to u0 (map-get? historical-fees height))))
    (if (> fee u0)
      {total: (+ (get total acc) fee), count: (+ (get count acc) u1)}
      acc)))

(define-private (get-recent-fees (current-height uint) (lookback uint))
  (let ((start-height (if (> current-height lookback)
                          (- current-height lookback)
                          u0)))
    ;; Placeholder for actual implementation
    u0))

;; Admin functions with enhanced security
(define-public (update-fee-data (height uint) (median-fee uint) (nonce uint))
  (begin
    (asserts! (is-admin) (err err-unauthorized))
    (asserts! (check-contract-active) (err err-contract-paused))
    (asserts! (check-and-update-nonce tx-sender nonce) (err err-invalid-nonce))
    
    ;; Check valid parameters
    (asserts! (> height u0) (err err-invalid-params))
    (asserts! (>= median-fee u0) (err err-invalid-params))
    
    ;; Update the data
    (map-set historical-fees height median-fee)
    (var-set last-update-height height)
    (ok true)))

(define-public (set-transaction-type-cost (tx-type (string-ascii 20)) (cost uint) (nonce uint))
  (begin
    (asserts! (is-admin) (err err-unauthorized))
    (asserts! (check-contract-active) (err err-contract-paused))
    (asserts! (check-and-update-nonce tx-sender nonce) (err err-invalid-nonce))
    
    ;; Check valid parameters
    (asserts! (> (len tx-type) u0) (err err-invalid-params))
    (asserts! (>= cost u0) (err err-invalid-params))
    
    ;; Update the data
    (map-set transaction-types tx-type cost)
    (ok true)))
