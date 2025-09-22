;; Community Innovation Lab - Prototype Testing Contract
;; Test and validate prototypes with community feedback

;; Data Variables
(define-data-var testing-session-counter uint u0)
(define-data-var feedback-counter uint u0)
(define-data-var admin principal tx-sender)

;; Constants
(define-constant ERR-UNAUTHORIZED (err u300))
(define-constant ERR-SESSION-NOT-FOUND (err u301))
(define-constant ERR-FEEDBACK-NOT-FOUND (err u302))
(define-constant ERR-INVALID-STATUS (err u303))
(define-constant ERR-SESSION-NOT-ACTIVE (err u304))
(define-constant ERR-ALREADY-PARTICIPATED (err u305))
(define-constant ERR-INVALID-RATING (err u306))
(define-constant ERR-SESSION-ENDED (err u307))

;; Session Status
(define-constant STATUS-PLANNED u1)
(define-constant STATUS-ACTIVE u2)
(define-constant STATUS-COMPLETED u3)
(define-constant STATUS-ANALYSED u4)
(define-constant STATUS-CANCELLED u5)

;; Feedback Types
(define-constant FEEDBACK-USABILITY u1)
(define-constant FEEDBACK-FUNCTIONALITY u2)
(define-constant FEEDBACK-DESIGN u3)
(define-constant FEEDBACK-OVERALL u4)
(define-constant FEEDBACK-SUGGESTION u5)

;; Testing Session Data Structure
(define-map testing-sessions uint {
    id: uint,
    project-id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    prototype-version: (string-ascii 50),
    organizer: principal,
    status: uint,
    start-date: uint,
    end-date: uint,
    max-participants: uint,
    current-participants: uint,
    total-feedback-count: uint,
    average-rating: uint,
    success-threshold: uint,
    created-at: uint,
    updated-at: uint
})

;; Feedback Data Structure
(define-map feedback uint {
    id: uint,
    session-id: uint,
    tester: principal,
    feedback-type: uint,
    rating: uint,
    comments: (string-ascii 500),
    is-positive: bool,
    weight: uint,
    submitted-at: uint
})

;; Session Participants
(define-map session-participants {session-id: uint, participant: principal} {
    joined-at: uint,
    feedback-submitted: bool,
    testing-experience: (string-ascii 50),
    completion-status: uint
})

;; Success Metrics
(define-map success-metrics {session-id: uint, metric-type: (string-ascii 50)} {
    target-value: uint,
    actual-value: uint,
    weight: uint,
    achieved: bool
})

;; Validation Scores
(define-map validation-scores {session-id: uint, category: (string-ascii 50)} {
    total-score: uint,
    feedback-count: uint,
    average-score: uint,
    meets-threshold: bool
})

;; Tester Qualifications
(define-map tester-profiles principal {
    experience-level: uint,
    expertise-areas: (string-ascii 200),
    sessions-completed: uint,
    average-feedback-quality: uint,
    active: bool
})

;; Create testing session
(define-public (create-testing-session (project-id uint) (title (string-ascii 100)) (description (string-ascii 500)) 
                                      (prototype-version (string-ascii 50)) (start-date uint) (end-date uint) 
                                      (max-participants uint) (success-threshold uint))
    (let (
        (session-id (+ (var-get testing-session-counter) u1))
        (current-block-height stacks-block-height)
    )
        (map-set testing-sessions session-id {
            id: session-id,
            project-id: project-id,
            title: title,
            description: description,
            prototype-version: prototype-version,
            organizer: tx-sender,
            status: STATUS-PLANNED,
            start-date: start-date,
            end-date: end-date,
            max-participants: max-participants,
            current-participants: u0,
            total-feedback-count: u0,
            average-rating: u0,
            success-threshold: success-threshold,
            created-at: current-block-height,
            updated-at: current-block-height
        })
        (var-set testing-session-counter session-id)
        (ok session-id)
    )
)

;; Join testing session
(define-public (join-testing-session (session-id uint) (testing-experience (string-ascii 50)))
    (let (
        (session (unwrap! (map-get? testing-sessions session-id) ERR-SESSION-NOT-FOUND))
        (participant-key {session-id: session-id, participant: tx-sender})
    )
        (asserts! (< (get current-participants session) (get max-participants session)) ERR-SESSION-NOT-ACTIVE)
        (asserts! (is-none (map-get? session-participants participant-key)) ERR-ALREADY-PARTICIPATED)
        (asserts! (is-eq (get status session) STATUS-ACTIVE) ERR-SESSION-NOT-ACTIVE)
        
        (map-set session-participants participant-key {
            joined-at: stacks-block-height,
            feedback-submitted: false,
            testing-experience: testing-experience,
            completion-status: u0
        })
        
        (map-set testing-sessions session-id (merge session {
            current-participants: (+ (get current-participants session) u1),
            updated-at: stacks-block-height
        }))
        
        (ok true)
    )
)

;; Submit feedback
(define-public (submit-feedback (session-id uint) (feedback-type uint) (rating uint) (comments (string-ascii 500)) (is-positive bool))
    (let (
        (session (unwrap! (map-get? testing-sessions session-id) ERR-SESSION-NOT-FOUND))
        (participant-key {session-id: session-id, participant: tx-sender})
        (participant (unwrap! (map-get? session-participants participant-key) ERR-UNAUTHORIZED))
        (feedback-id (+ (var-get feedback-counter) u1))
        (tester-profile (default-to {experience-level: u1, expertise-areas: "", sessions-completed: u0, 
                                    average-feedback-quality: u5, active: true} 
                                    (map-get? tester-profiles tx-sender)))
        (feedback-weight (get experience-level tester-profile))
    )
        (asserts! (is-eq (get status session) STATUS-ACTIVE) ERR-SESSION-NOT-ACTIVE)
        (asserts! (<= stacks-block-height (get end-date session)) ERR-SESSION-ENDED)
        (asserts! (and (>= rating u1) (<= rating u10)) ERR-INVALID-RATING)
        
        ;; Record feedback
        (map-set feedback feedback-id {
            id: feedback-id,
            session-id: session-id,
            tester: tx-sender,
            feedback-type: feedback-type,
            rating: rating,
            comments: comments,
            is-positive: is-positive,
            weight: feedback-weight,
            submitted-at: stacks-block-height
        })
        
        ;; Update participant status
        (map-set session-participants participant-key (merge participant {
            feedback-submitted: true,
            completion-status: u1
        }))
        
        ;; Update session metrics
        (let (
            (new-feedback-count (+ (get total-feedback-count session) u1))
            (current-total-rating (* (get average-rating session) (get total-feedback-count session)))
            (new-average-rating (/ (+ current-total-rating rating) new-feedback-count))
        )
            (map-set testing-sessions session-id (merge session {
                total-feedback-count: new-feedback-count,
                average-rating: new-average-rating,
                updated-at: stacks-block-height
            }))
        )
        
        (var-set feedback-counter feedback-id)
        (ok feedback-id)
    )
)

;; Start testing session
(define-public (start-testing-session (session-id uint))
    (let (
        (session (unwrap! (map-get? testing-sessions session-id) ERR-SESSION-NOT-FOUND))
    )
        (asserts! (or (is-eq tx-sender (get organizer session)) (is-eq tx-sender (var-get admin))) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status session) STATUS-PLANNED) ERR-INVALID-STATUS)
        
        (map-set testing-sessions session-id (merge session {
            status: STATUS-ACTIVE,
            updated-at: stacks-block-height
        }))
        
        (ok true)
    )
)

;; Complete testing session
(define-public (complete-testing-session (session-id uint))
    (let (
        (session (unwrap! (map-get? testing-sessions session-id) ERR-SESSION-NOT-FOUND))
    )
        (asserts! (or (is-eq tx-sender (get organizer session)) (is-eq tx-sender (var-get admin))) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status session) STATUS-ACTIVE) ERR-INVALID-STATUS)
        
        (map-set testing-sessions session-id (merge session {
            status: STATUS-COMPLETED,
            updated-at: stacks-block-height
        }))
        
        (ok true)
    )
)

;; Set success metric
(define-public (set-success-metric (session-id uint) (metric-type (string-ascii 50)) (target-value uint) (weight uint))
    (let (
        (session (unwrap! (map-get? testing-sessions session-id) ERR-SESSION-NOT-FOUND))
    )
        (asserts! (or (is-eq tx-sender (get organizer session)) (is-eq tx-sender (var-get admin))) ERR-UNAUTHORIZED)
        
        (map-set success-metrics {session-id: session-id, metric-type: metric-type} {
            target-value: target-value,
            actual-value: u0,
            weight: weight,
            achieved: false
        })
        
        (ok true)
    )
)

;; Update validation score
(define-public (update-validation-score (session-id uint) (category (string-ascii 50)) (total-score uint) (feedback-count uint))
    (let (
        (session (unwrap! (map-get? testing-sessions session-id) ERR-SESSION-NOT-FOUND))
        (average-score (if (> feedback-count u0) (/ total-score feedback-count) u0))
        (meets-threshold (>= average-score (get success-threshold session)))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        
        (map-set validation-scores {session-id: session-id, category: category} {
            total-score: total-score,
            feedback-count: feedback-count,
            average-score: average-score,
            meets-threshold: meets-threshold
        })
        
        (ok meets-threshold)
    )
)

;; Register as tester
(define-public (register-as-tester (experience-level uint) (expertise-areas (string-ascii 200)))
    (begin
        (asserts! (and (>= experience-level u1) (<= experience-level u5)) ERR-INVALID-RATING)
        
        (map-set tester-profiles tx-sender {
            experience-level: experience-level,
            expertise-areas: expertise-areas,
            sessions-completed: u0,
            average-feedback-quality: u5,
            active: true
        })
        
        (ok true)
    )
)

;; Analyse session results
(define-public (analyse-session (session-id uint))
    (let (
        (session (unwrap! (map-get? testing-sessions session-id) ERR-SESSION-NOT-FOUND))
    )
        (asserts! (or (is-eq tx-sender (get organizer session)) (is-eq tx-sender (var-get admin))) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status session) STATUS-COMPLETED) ERR-INVALID-STATUS)
        
        (map-set testing-sessions session-id (merge session {
            status: STATUS-ANALYSED,
            updated-at: stacks-block-height
        }))
        
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-testing-session (session-id uint))
    (map-get? testing-sessions session-id)
)

(define-read-only (get-feedback (feedback-id uint))
    (map-get? feedback feedback-id)
)

(define-read-only (get-session-participant (session-id uint) (participant principal))
    (map-get? session-participants {session-id: session-id, participant: participant})
)

(define-read-only (get-success-metric (session-id uint) (metric-type (string-ascii 50)))
    (map-get? success-metrics {session-id: session-id, metric-type: metric-type})
)

(define-read-only (get-validation-score (session-id uint) (category (string-ascii 50)))
    (map-get? validation-scores {session-id: session-id, category: category})
)

(define-read-only (get-tester-profile (tester principal))
    (map-get? tester-profiles tester)
)

(define-read-only (get-session-count)
    (var-get testing-session-counter)
)

(define-read-only (get-feedback-count)
    (var-get feedback-counter)
)

(define-read-only (get-admin)
    (var-get admin)
)

;; Check if session meets success criteria
(define-read-only (session-meets-success-criteria (session-id uint))
    (match (map-get? testing-sessions session-id)
        session (>= (get average-rating session) (get success-threshold session))
        false
    )
)
