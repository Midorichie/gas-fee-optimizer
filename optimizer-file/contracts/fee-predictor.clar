;; Fee Predictor Contract
;; Predicts future gas fees based on time patterns

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-params (err u101))
(define-constant seconds-per-day u86400)
(define-constant seconds-per-hour u3600)

;; Data variables
(define-data-var last-updated-time uint u0)

;; Maps
(define-map hourly-fee-patterns uint uint)  ;; hour-of-day (0-23) -> average fee multiplier (basis points)
(define-map daily-fee-patterns uint uint)   ;; day-of-week (0-6) -> average fee multiplier (basis points)

;; Public functions
(define-read-only (predict-fee (base-fee uint) (future-time-offset uint))
  (let (
    (current-time burn-block-height)
    (future-time (+ current-time future-time-offset))
    (hour-multiplier (get-hour-multiplier future-time))
    (day-multiplier (get-day-multiplier future-time))
    (combined-multiplier (/ (+ hour-multiplier day-multiplier) u2))
  )
    ;; Apply combined multiplier to base fee
    ;; 10000 basis points = 100% (no change)
    (/ (* base-fee combined-multiplier) u10000)))

;; Helper functions
(define-read-only (get-hour-multiplier (timestamp uint))
  (let ((hour-of-day (get-hour-from-timestamp timestamp)))
    (default-to u10000 (map-get? hourly-fee-patterns hour-of-day))))

(define-read-only (get-day-multiplier (timestamp uint))
  (let ((day-of-week (get-day-from-timestamp timestamp)))
    (default-to u10000 (map-get? daily-fee-patterns day-of-week))))

(define-read-only (get-hour-from-timestamp (timestamp uint))
  (mod (/ timestamp seconds-per-hour) u24))

(define-read-only (get-day-from-timestamp (timestamp uint))
  (mod (/ timestamp seconds-per-day) u7))

;; Admin functions
(define-public (set-hourly-pattern (hour uint) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (< hour u24) err-invalid-params)
    (asserts! (> multiplier u0) err-invalid-params)
    (map-set hourly-fee-patterns hour multiplier)
    (ok true)))

(define-public (set-daily-pattern (day uint) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (< day u7) err-invalid-params)
    (asserts! (> multiplier u0) err-invalid-params)
    (map-set daily-fee-patterns day multiplier)
    (ok true)))

(define-public (set-initial-patterns)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Set hourly patterns (example: higher fees during business hours)
    (map-set hourly-fee-patterns u0 u8500)   ;; 12am - 85% of standard fee
    (map-set hourly-fee-patterns u1 u8000)   ;; 1am  - 80% of standard fee
    (map-set hourly-fee-patterns u2 u7500)   ;; 2am  - 75% of standard fee
    (map-set hourly-fee-patterns u3 u7000)   ;; 3am  - 70% of standard fee
    (map-set hourly-fee-patterns u4 u7500)   ;; 4am  - 75% of standard fee
    (map-set hourly-fee-patterns u5 u8000)   ;; 5am  - 80% of standard fee
    (map-set hourly-fee-patterns u6 u9000)   ;; 6am  - 90% of standard fee
    (map-set hourly-fee-patterns u7 u10000)  ;; 7am  - 100% of standard fee
    (map-set hourly-fee-patterns u8 u11000)  ;; 8am  - 110% of standard fee
    (map-set hourly-fee-patterns u9 u12000)  ;; 9am  - 120% of standard fee
    (map-set hourly-fee-patterns u10 u12500) ;; 10am - 125% of standard fee
    (map-set hourly-fee-patterns u11 u12000) ;; 11am - 120% of standard fee
    (map-set hourly-fee-patterns u12 u11000) ;; 12pm - 110% of standard fee
    (map-set hourly-fee-patterns u13 u11500) ;; 1pm  - 115% of standard fee
    (map-set hourly-fee-patterns u14 u12000) ;; 2pm  - 120% of standard fee
    (map-set hourly-fee-patterns u15 u12500) ;; 3pm  - 125% of standard fee
    (map-set hourly-fee-patterns u16 u13000) ;; 4pm  - 130% of standard fee
    (map-set hourly-fee-patterns u17 u12000) ;; 5pm  - 120% of standard fee
    (map-set hourly-fee-patterns u18 u11000) ;; 6pm  - 110% of standard fee
    (map-set hourly-fee-patterns u19 u10500) ;; 7pm  - 105% of standard fee
    (map-set hourly-fee-patterns u20 u10000) ;; 8pm  - 100% of standard fee
    (map-set hourly-fee-patterns u21 u9500)  ;; 9pm  - 95% of standard fee
    (map-set hourly-fee-patterns u22 u9000)  ;; 10pm - 90% of standard fee
    (map-set hourly-fee-patterns u23 u8500)  ;; 11pm - 85% of standard fee
    
    ;; Set daily patterns (example: higher fees on weekdays)
    (map-set daily-fee-patterns u0 u9000)    ;; Sunday    - 90% of standard fee
    (map-set daily-fee-patterns u1 u11000)   ;; Monday    - 110% of standard fee
    (map-set daily-fee-patterns u2 u11000)   ;; Tuesday   - 110% of standard fee
    (map-set daily-fee-patterns u3 u11000)   ;; Wednesday - 110% of standard fee
    (map-set daily-fee-patterns u4 u11000)   ;; Thursday  - 110% of standard fee
    (map-set daily-fee-patterns u5 u10500)   ;; Friday    - 105% of standard fee
    (map-set daily-fee-patterns u6 u9000)    ;; Saturday  - 90% of standard fee
    
    (var-set last-updated-time burn-block-height)
    (ok true)))
