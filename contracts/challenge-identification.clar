;; Community Innovation Lab - Challenge Identification Contract
;; Identify and prioritize community challenges for innovation

;; Data Variables
(define-data-var challenge-counter uint u0)
(define-data-var admin principal tx-sender)

;; Constants
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-CHALLENGE-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-VOTED (err u102))
(define-constant ERR-INVALID-STATUS (err u103))
(define-constant ERR-INSUFFICIENT-VOTES (err u104))

;; Challenge Status
(define-constant STATUS-SUBMITTED u1)
(define-constant STATUS-VOTING u2)
(define-constant STATUS-APPROVED u3)
(define-constant STATUS-REJECTED u4)
(define-constant STATUS-IN-PROGRESS u5)
(define-constant STATUS-COMPLETED u6)

;; Challenge Data Structure
(define-map challenges uint {
    id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    submitter: principal,
    priority-score: uint,
    vote-count: uint,
    status: uint,
    category: (string-ascii 50),
    created-at: uint,
    updated-at: uint
})

;; Voting Records
(define-map votes {challenge-id: uint, voter: principal} {
    vote-weight: uint,
    voted-at: uint
})

;; User Voting Power
(define-map user-voting-power principal uint)

;; Challenge Categories
(define-map categories (string-ascii 50) {
    name: (string-ascii 50),
    description: (string-ascii 200),
    active: bool
})

;; Priority Weights by Category
(define-map category-weights (string-ascii 50) uint)

;; Submit a new challenge
(define-public (submit-challenge (title (string-ascii 100)) (description (string-ascii 500)) (category (string-ascii 50)))
    (let (
        (challenge-id (+ (var-get challenge-counter) u1))
        (current-block-height stacks-block-height)
    )
        (map-set challenges challenge-id {
            id: challenge-id,
            title: title,
            description: description,
            submitter: tx-sender,
            priority-score: u0,
            vote-count: u0,
            status: STATUS-SUBMITTED,
            category: category,
            created-at: current-block-height,
            updated-at: current-block-height
        })
        (var-set challenge-counter challenge-id)
        (ok challenge-id)
    )
)

;; Vote on a challenge
(define-public (vote-on-challenge (challenge-id uint) (vote-weight uint))
    (let (
        (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
        (voter tx-sender)
        (voter-power (default-to u1 (map-get? user-voting-power voter)))
        (effective-vote-weight (if (<= vote-weight voter-power) vote-weight voter-power))
    )
        ;; Check if user already voted
        (asserts! (is-none (map-get? votes {challenge-id: challenge-id, voter: voter})) ERR-ALREADY-VOTED)
        
        ;; Record vote
        (map-set votes {challenge-id: challenge-id, voter: voter} {
            vote-weight: effective-vote-weight,
            voted-at: stacks-block-height
        })
        
        ;; Update challenge with new vote
        (map-set challenges challenge-id (merge challenge {
            priority-score: (+ (get priority-score challenge) effective-vote-weight),
            vote-count: (+ (get vote-count challenge) u1),
            updated-at: stacks-block-height
        }))
        
        (ok effective-vote-weight)
    )
)

;; Update challenge status
(define-public (update-challenge-status (challenge-id uint) (new-status uint))
    (let (
        (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    )
        ;; Only admin can update status
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        
        ;; Validate status
        (asserts! (and (>= new-status STATUS-SUBMITTED) (<= new-status STATUS-COMPLETED)) ERR-INVALID-STATUS)
        
        ;; Update challenge status
        (map-set challenges challenge-id (merge challenge {
            status: new-status,
            updated-at: stacks-block-height
        }))
        
        (ok true)
    )
)

;; Set user voting power
(define-public (set-voting-power (user principal) (power uint))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        (map-set user-voting-power user power)
        (ok true)
    )
)

;; Add or update category
(define-public (manage-category (name (string-ascii 50)) (description (string-ascii 200)) (weight uint) (active bool))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        (map-set categories name {
            name: name,
            description: description,
            active: active
        })
        (map-set category-weights name weight)
        (ok true)
    )
)

;; Approve challenge for incubation
(define-public (approve-for-incubation (challenge-id uint))
    (let (
        (challenge (unwrap! (map-get? challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        (asserts! (>= (get priority-score challenge) u10) ERR-INSUFFICIENT-VOTES)
        
        (map-set challenges challenge-id (merge challenge {
            status: STATUS-APPROVED,
            updated-at: stacks-block-height
        }))
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-challenge (challenge-id uint))
    (map-get? challenges challenge-id)
)

(define-read-only (get-vote (challenge-id uint) (voter principal))
    (map-get? votes {challenge-id: challenge-id, voter: voter})
)

(define-read-only (get-user-voting-power (user principal))
    (default-to u1 (map-get? user-voting-power user))
)

(define-read-only (get-category (name (string-ascii 50)))
    (map-get? categories name)
)

(define-read-only (get-category-weight (name (string-ascii 50)))
    (default-to u1 (map-get? category-weights name))
)

(define-read-only (get-challenge-count)
    (var-get challenge-counter)
)

(define-read-only (get-admin)
    (var-get admin)
)

;; Check if challenge meets approval threshold
(define-read-only (is-challenge-ready-for-approval (challenge-id uint))
    (match (map-get? challenges challenge-id)
        challenge (>= (get priority-score challenge) u10)
        false
    )
)
