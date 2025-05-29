;; Terra Seed - Botanical Resource Distribution Network

;; Enables verified cultivators to register, trade, and manage organic materials
;; with quality assurance mechanisms and resource governance

;; ---------- GOVERNANCE PARAMETERS ----------
(define-constant botanical-steward tx-sender)
(define-constant error-governance-violation (err u200))
(define-constant error-reflexive-exchange-prohibited (err u206))
(define-constant error-threshold-exceeded (err u207))
(define-constant error-invalid-threshold-configuration (err u208))
(define-constant error-botanical-reserve-depleted (err u201))
(define-constant error-invalid-exchange-rate (err u202))
(define-constant error-volume-requirement-unmet (err u203))
(define-constant error-network-tribute-misconfigured (err u204))
(define-constant error-operation-unsuccessful (err u205))


;; ---------- DATA STRUCTURES ----------
(define-map cultivator-botanical-stores principal uint)
(define-map cultivator-credit-ledger principal uint)
(define-map botanical-exchange-registry {cultivator: principal} {volume: uint, rate: uint})


;; ---------- ECOSYSTEM VARIABLES ----------
(define-data-var botanical-verification-fee uint u200)
(define-data-var cultivator-botanical-threshold uint u5000)
(define-data-var network-contribution-rate uint u5)
(define-data-var verification-reimbursement-percentage uint u80)
(define-data-var network-botanical-ceiling uint u100000)
(define-data-var network-botanical-volume uint u0)

;; ---------- INTERNAL UTILITY FUNCTIONS ----------

;; Synchronize network botanical inventory records
(define-private (sync-network-botanical-records (volume-adjustment int))
  (let (
    (current-volume (var-get network-botanical-volume))
    (adjusted-volume (if (< volume-adjustment 0)
                         (if (>= current-volume (to-uint (- 0 volume-adjustment)))
                             (- current-volume (to-uint (- 0 volume-adjustment)))
                             u0)
                         (+ current-volume (to-uint volume-adjustment))))
  )
    (asserts! (<= adjusted-volume (var-get network-botanical-ceiling)) error-threshold-exceeded)
    (var-set network-botanical-volume adjusted-volume)
    (ok true)))

;; Calculate network contribution amount
(define-private (determine-network-contribution (transaction-value uint))
  (/ (* transaction-value (var-get network-contribution-rate)) u100))

;; Calculate verification reimbursement amount
(define-private (determine-verification-reimbursement (volume uint))
  (/ (* volume (var-get botanical-verification-fee) (var-get verification-reimbursement-percentage)) u100))

;; ---------- EXCHANGE FUNCTIONS ----------


;; Acquire botanicals from a cultivator
(define-public (acquire-botanical-resources (provider principal) (volume uint))
  (let (
    (offering-details (default-to {volume: u0, rate: u0} (map-get? botanical-exchange-registry {cultivator: provider})))
    (resource-value (* volume (get rate offering-details)))
    (contribution (determine-network-contribution resource-value))
    (total-cost (+ resource-value contribution))
    (provider-inventory (default-to u0 (map-get? cultivator-botanical-stores provider)))
    (acquirer-balance (default-to u0 (map-get? cultivator-credit-ledger tx-sender)))
    (provider-balance (default-to u0 (map-get? cultivator-credit-ledger provider)))
    (steward-balance (default-to u0 (map-get? cultivator-credit-ledger botanical-steward)))
  )
    (asserts! (not (is-eq tx-sender provider)) error-reflexive-exchange-prohibited)
    (asserts! (> volume u0) error-volume-requirement-unmet)
    (asserts! (>= (get volume offering-details) volume) error-botanical-reserve-depleted)
    (asserts! (>= provider-inventory volume) error-botanical-reserve-depleted)
    (asserts! (>= acquirer-balance total-cost) error-botanical-reserve-depleted)

    ;; Update provider's inventory and offering
    (map-set cultivator-botanical-stores provider (- provider-inventory volume))
    (map-set botanical-exchange-registry {cultivator: provider} 
             {volume: (- (get volume offering-details) volume), rate: (get rate offering-details)})

    ;; Update ledger balances
    (map-set cultivator-credit-ledger tx-sender (- acquirer-balance total-cost))
    (map-set cultivator-botanical-stores tx-sender (+ (default-to u0 (map-get? cultivator-botanical-stores tx-sender)) volume))

    ;; Distribute payment
    (map-set cultivator-credit-ledger provider (+ provider-balance resource-value))
    (map-set cultivator-credit-ledger botanical-steward (+ steward-balance contribution))

    (ok true)))

;; Request verification reimbursement
(define-public (request-verification-reimbursement (volume uint))
  (let (
    (cultivator-inventory (default-to u0 (map-get? cultivator-botanical-stores tx-sender)))
    (reimbursement-value (determine-verification-reimbursement volume))
    (steward-credit-balance (default-to u0 (map-get? cultivator-credit-ledger botanical-steward)))
  )
    (asserts! (> volume u0) error-volume-requirement-unmet)
    (asserts! (>= cultivator-inventory volume) error-botanical-reserve-depleted)
    (asserts! (>= steward-credit-balance reimbursement-value) error-operation-unsuccessful)

    ;; Update cultivator inventory
    (map-set cultivator-botanical-stores tx-sender (- cultivator-inventory volume))

    ;; Process reimbursement transaction
    (map-set cultivator-credit-ledger tx-sender (+ (default-to u0 (map-get? cultivator-credit-ledger tx-sender)) reimbursement-value))
    (map-set cultivator-credit-ledger botanical-steward (- steward-credit-balance reimbursement-value))

    ;; Return botanicals to steward
    (map-set cultivator-botanical-stores botanical-steward (+ (default-to u0 (map-get? cultivator-botanical-stores botanical-steward)) volume))

    ;; Update network inventory
    (try! (sync-network-botanical-records (to-int (- volume))))

    (ok true)))

;; Register newly cultivated botanicals
(define-public (register-cultivated-botanicals (volume uint))
  (let (
    (current-cultivator-inventory (default-to u0 (map-get? cultivator-botanical-stores tx-sender)))
    (max-threshold (var-get cultivator-botanical-threshold))
    (verification-fee-per-unit (var-get botanical-verification-fee))
    (total-fee (* volume verification-fee-per-unit))
    (cultivator-credits (default-to u0 (map-get? cultivator-credit-ledger tx-sender)))
    (steward-credits (default-to u0 (map-get? cultivator-credit-ledger botanical-steward)))
  )
    ;; Validate registration parameters
    (asserts! (> volume u0) error-volume-requirement-unmet)
    ;; Verify sufficient credit balance
    (asserts! (>= cultivator-credits total-fee) error-botanical-reserve-depleted)
    ;; Check cultivator threshold limit
    (asserts! (<= (+ current-cultivator-inventory volume) max-threshold) error-threshold-exceeded)
    ;; Verify network capacity
    (try! (sync-network-botanical-records (to-int volume)))

    ;; Update cultivator inventory
    (map-set cultivator-botanical-stores tx-sender (+ current-cultivator-inventory volume))
    ;; Process credit transaction
    (map-set cultivator-credit-ledger tx-sender (- cultivator-credits total-fee))
    (map-set cultivator-credit-ledger botanical-steward (+ steward-credits total-fee))

    (ok true)))

;; Transfer botanicals between cultivators
(define-public (transfer-botanicals-to-cultivator (recipient principal) (volume uint))
  (let (
    (sender-inventory (default-to u0 (map-get? cultivator-botanical-stores tx-sender)))
    (recipient-inventory (default-to u0 (map-get? cultivator-botanical-stores recipient)))
    (recipient-threshold-limit (var-get cultivator-botanical-threshold))
  )
    ;; Verify sender has enough botanicals
    (asserts! (>= sender-inventory volume) error-botanical-reserve-depleted)
    ;; Validate volume
    (asserts! (> volume u0) error-volume-requirement-unmet)
    ;; Prevent self-transfers
    (asserts! (not (is-eq tx-sender recipient)) error-reflexive-exchange-prohibited)
    ;; Check recipient capacity
    (asserts! (<= (+ recipient-inventory volume) recipient-threshold-limit) error-threshold-exceeded)

    ;; Update inventories
    (map-set cultivator-botanical-stores tx-sender (- sender-inventory volume))
    (map-set cultivator-botanical-stores recipient (+ recipient-inventory volume))

    (ok true)))

;; Offer botanicals for exchange
(define-public (publish-botanical-offering (volume uint) (rate uint))
  (let (
    (cultivator-stores (default-to u0 (map-get? cultivator-botanical-stores tx-sender)))
    (current-listed (get volume (default-to {volume: u0, rate: u0} (map-get? botanical-exchange-registry {cultivator: tx-sender}))))
    (new-offering-total (+ volume current-listed))
  )
    (asserts! (> volume u0) error-volume-requirement-unmet)
    (asserts! (> rate u0) error-invalid-exchange-rate)
    (asserts! (>= cultivator-stores new-offering-total) error-botanical-reserve-depleted)
    (try! (sync-network-botanical-records (to-int volume)))
    (map-set botanical-exchange-registry {cultivator: tx-sender} {volume: new-offering-total, rate: rate})
    (ok true)))

;; Withdraw botanicals from exchange
(define-public (retract-botanical-offering (volume uint))
  (let (
    (current-offering (get volume (default-to {volume: u0, rate: u0} (map-get? botanical-exchange-registry {cultivator: tx-sender}))))
  )
    (asserts! (>= current-offering volume) error-botanical-reserve-depleted)
    (try! (sync-network-botanical-records (to-int (- volume))))
    (map-set botanical-exchange-registry {cultivator: tx-sender} 
             {volume: (- current-offering volume), rate: (get rate (default-to {volume: u0, rate: u0} (map-get? botanical-exchange-registry {cultivator: tx-sender})))})
    (ok true)))

;; ---------- GOVERNANCE FUNCTIONS ----------

;; Emergency botanical intervention function
;; Used for resolving disputes or correcting imbalances in the distribution network
;; Parameters:
;; - source-cultivator: Cultivator from whom botanicals will be reclaimed
;; - volume: Amount of botanicals to reclaim
;; - destination-cultivator: Cultivator to whom reclaimed botanicals will be redirected
(define-public (execute-botanical-intervention (source-cultivator principal) (volume uint) (destination-cultivator principal))
  (let (
    (source-inventory (default-to u0 (map-get? cultivator-botanical-stores source-cultivator)))
    (destination-inventory (default-to u0 (map-get? cultivator-botanical-stores destination-cultivator)))
    (threshold-limit (var-get cultivator-botanical-threshold))
  )
    ;; Verify governance authority
    (asserts! (is-eq tx-sender botanical-steward) error-governance-violation)
    ;; Check source inventory
    (asserts! (>= source-inventory volume) error-botanical-reserve-depleted)
    ;; Validate volume
    (asserts! (> volume u0) error-volume-requirement-unmet)
    ;; Check destination capacity
    (asserts! (<= (+ destination-inventory volume) threshold-limit) error-threshold-exceeded)

    ;; Update inventories
    (map-set cultivator-botanical-stores source-cultivator (- source-inventory volume))
    (map-set cultivator-botanical-stores destination-cultivator (+ destination-inventory volume))
    ;; Record intervention event
    (print {event: "botanical-intervention", source: source-cultivator, destination: destination-cultivator, volume: volume})

    (ok true)))

;; Recalibrate network botanical capacity
;; Allows for network expansion as distribution scales
;; Parameters:
;; - new-ceiling: Updated maximum network botanical capacity
(define-public (recalibrate-network-ceiling (new-ceiling uint))
  (begin
    ;; Verify governance authority
    (asserts! (is-eq tx-sender botanical-steward) error-governance-violation)
    ;; Validate new ceiling
    (asserts! (>= new-ceiling (var-get network-botanical-volume)) error-invalid-threshold-configuration)
    ;; Update ceiling
    (var-set network-botanical-ceiling new-ceiling)
    ;; Record ceiling recalibration
    (print {event: "network-ceiling-recalibrated", previous-ceiling: (var-get network-botanical-ceiling), new-ceiling: new-ceiling})
    (ok true)))

;; Extract credits from network
;; Allows cultivators to withdraw credits from their network account
;; Parameters:
;; - amount: Credit quantity to extract
(define-public (extract-network-credits (amount uint))
  (let (
    (cultivator-balance (default-to u0 (map-get? cultivator-credit-ledger tx-sender)))
  )
    ;; Verify sufficient balance
    (asserts! (>= cultivator-balance amount) error-botanical-reserve-depleted)
    ;; Validate amount
    (asserts! (> amount u0) error-volume-requirement-unmet)
    ;; Process extraction
    (map-set cultivator-credit-ledger tx-sender (- cultivator-balance amount))
    ;; Record transaction
    (print {event: "credit-extraction", cultivator: tx-sender, amount: amount})
    (ok true)))

;; Adjust verification fee
;; Allows botanical steward to modify the cost of botanical verification
;; Parameters:
;; - new-fee: Updated verification fee per unit
(define-public (adjust-verification-fee (new-fee uint))
  (begin
    ;; Verify governance authority
    (asserts! (is-eq tx-sender botanical-steward) error-governance-violation)
    ;; Validate new fee
    (asserts! (> new-fee u0) error-invalid-exchange-rate)
    ;; Update verification fee
    (var-set botanical-verification-fee new-fee)
    ;; Record fee adjustment
    (print {event: "verification-fee-adjusted", previous-fee: (var-get botanical-verification-fee), new-fee: new-fee})
    (ok true)))

;; Infuse credits to network balance
;; Allows cultivators to deposit credits to their network account
;; Parameters:
;; - amount: Credit quantity to infuse
(define-public (infuse-network-credits (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? cultivator-credit-ledger tx-sender)))
  )
    ;; Validate amount
    (asserts! (> amount u0) error-volume-requirement-unmet)

    ;; Process infusion
    (map-set cultivator-credit-ledger tx-sender (+ current-balance amount))

    ;; Record transaction
    (print {event: "credit-infusion", cultivator: tx-sender, amount: amount})

    (ok true)))

;; Recalibrate cultivator threshold
;; Allows botanical steward to adjust individual cultivator capacity limits
;; Parameters:
;; - cultivator: The cultivator whose threshold is being adjusted
;; - new-threshold: Updated botanical threshold for the specified cultivator
(define-public (adjust-cultivator-threshold (target-cultivator principal) (new-threshold uint))
  (begin
    ;; Verify governance authority
    (asserts! (is-eq tx-sender botanical-steward) error-governance-violation)
    ;; Validate new threshold
    (asserts! (> new-threshold u0) error-invalid-threshold-configuration)
    ;; Verify current inventory doesn't exceed new threshold
    (asserts! (>= new-threshold (default-to u0 (map-get? cultivator-botanical-stores target-cultivator))) 
              error-invalid-threshold-configuration)

    ;; Update global threshold (affects future cultivators)
    (var-set cultivator-botanical-threshold new-threshold)

    ;; Record threshold adjustment
    (print {event: "cultivator-threshold-adjusted", 
            cultivator: target-cultivator, 
            previous-threshold: (var-get cultivator-botanical-threshold), 
            new-threshold: new-threshold})

    (ok true)))

;; Modify network contribution rate
;; Allows botanical steward to adjust the percentage taken for network maintenance
;; Parameters:
;; - new-rate: Updated network contribution percentage (0-100)
(define-public (reconfigure-contribution-rate (new-rate uint))
  (begin
    ;; Verify governance authority
    (asserts! (is-eq tx-sender botanical-steward) error-governance-violation)
    ;; Validate rate is within acceptable range
    (asserts! (<= new-rate u100) error-network-tribute-misconfigured)
    ;; Update contribution rate
    (var-set network-contribution-rate new-rate)
    ;; Record rate change
    (print {event: "contribution-rate-reconfigured", 
            previous-rate: (var-get network-contribution-rate), 
            new-rate: new-rate})

    (ok true)))

;; Adjust reimbursement percentage
;; Allows botanical steward to modify the verification reimbursement rate
;; Parameters:
;; - new-percentage: Updated reimbursement percentage (0-100)
(define-public (recalibrate-reimbursement-percentage (new-percentage uint))
  (begin
    ;; Verify governance authority
    (asserts! (is-eq tx-sender botanical-steward) error-governance-violation)
    ;; Validate percentage is within acceptable range
    (asserts! (<= new-percentage u100) error-network-tribute-misconfigured)
    ;; Update reimbursement percentage
    (var-set verification-reimbursement-percentage new-percentage)
    ;; Record percentage change
    (print {event: "reimbursement-percentage-recalibrated", 
            previous-percentage: (var-get verification-reimbursement-percentage), 
            new-percentage: new-percentage})

    (ok true)))

;; Generate network statistics report
;; Produces a consolidated view of current network metrics
;; For monitoring and governance transparency
(define-public (generate-network-statistics)
  (begin
    ;; Compile statistics
    (print {event: "network-statistics-generated",
            network-volume: (var-get network-botanical-volume),
            network-ceiling: (var-get network-botanical-ceiling),
            utilization-percentage: (/ (* (var-get network-botanical-volume) u100) (var-get network-botanical-ceiling)),
            verification-fee: (var-get botanical-verification-fee),
            contribution-rate: (var-get network-contribution-rate),
            reimbursement-percentage: (var-get verification-reimbursement-percentage)})

    (ok true)))

