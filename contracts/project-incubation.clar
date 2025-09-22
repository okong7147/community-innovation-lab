;; Community Innovation Lab - Project Incubation Contract
;; Incubate innovative solutions with community resources

;; Data Variables
(define-data-var project-counter uint u0)
(define-data-var admin principal tx-sender)
(define-data-var total-community-funds uint u0)

;; Constants
(define-constant ERR-UNAUTHORIZED (err u200))
(define-constant ERR-PROJECT-NOT-FOUND (err u201))
(define-constant ERR-INSUFFICIENT-FUNDS (err u202))
(define-constant ERR-INVALID-STATUS (err u203))
(define-constant ERR-MILESTONE-NOT-FOUND (err u204))
(define-constant ERR-ALREADY-MENTOR (err u205))
(define-constant ERR-NOT-PROJECT-OWNER (err u206))
(define-constant ERR-INVALID-MILESTONE (err u207))

;; Project Status
(define-constant STATUS-PROPOSED u1)
(define-constant STATUS-UNDER-REVIEW u2)
(define-constant STATUS-APPROVED u3)
(define-constant STATUS-IN-PROGRESS u4)
(define-constant STATUS-COMPLETED u5)
(define-constant STATUS-CANCELLED u6)

;; Milestone Status
(define-constant MILESTONE-PENDING u1)
(define-constant MILESTONE-IN-PROGRESS u2)
(define-constant MILESTONE-COMPLETED u3)
(define-constant MILESTONE-VERIFIED u4)

;; Project Data Structure
(define-map projects uint {
    id: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    challenge-id: uint,
    proposer: principal,
    status: uint,
    total-funding-requested: uint,
    total-funding-allocated: uint,
    total-funding-released: uint,
    mentor-count: uint,
    milestone-count: uint,
    created-at: uint,
    updated-at: uint
})

;; Milestone Data Structure
(define-map milestones {project-id: uint, milestone-id: uint} {
    id: uint,
    title: (string-ascii 100),
    description: (string-ascii 300),
    funding-amount: uint,
    status: uint,
    due-date: uint,
    completed-at: (optional uint),
    verification-notes: (optional (string-ascii 300))
})

;; Resource Allocation
(define-map resource-allocations {project-id: uint, resource-type: (string-ascii 50)} {
    amount: uint,
    allocated-at: uint,
    notes: (string-ascii 200)
})

;; Mentor Assignments
(define-map mentors {project-id: uint, mentor: principal} {
    expertise-area: (string-ascii 50),
    assigned-at: uint,
    active: bool
})

;; Community Resource Pool
(define-map community-resources (string-ascii 50) {
    total-available: uint,
    allocated: uint,
    resource-type: (string-ascii 50),
    description: (string-ascii 200)
})

;; Submit project proposal
(define-public (submit-project-proposal (title (string-ascii 100)) (description (string-ascii 500)) (challenge-id uint) (funding-requested uint))
    (let (
        (project-id (+ (var-get project-counter) u1))
        (current-block-height stacks-block-height)
    )
        (map-set projects project-id {
            id: project-id,
            title: title,
            description: description,
            challenge-id: challenge-id,
            proposer: tx-sender,
            status: STATUS-PROPOSED,
            total-funding-requested: funding-requested,
            total-funding-allocated: u0,
            total-funding-released: u0,
            mentor-count: u0,
            milestone-count: u0,
            created-at: current-block-height,
            updated-at: current-block-height
        })
        (var-set project-counter project-id)
        (ok project-id)
    )
)

;; Approve project for incubation
(define-public (approve-project (project-id uint) (allocated-funding uint))
    (let (
        (project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        (asserts! (<= allocated-funding (var-get total-community-funds)) ERR-INSUFFICIENT-FUNDS)
        
        (map-set projects project-id (merge project {
            status: STATUS-APPROVED,
            total-funding-allocated: allocated-funding,
            updated-at: stacks-block-height
        }))
        
        (var-set total-community-funds (- (var-get total-community-funds) allocated-funding))
        (ok true)
    )
)

;; Add milestone to project
(define-public (add-milestone (project-id uint) (title (string-ascii 100)) (description (string-ascii 300)) (funding-amount uint) (due-date uint))
    (let (
        (project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
        (milestone-id (+ (get milestone-count project) u1))
    )
        ;; Only project owner or admin can add milestones
        (asserts! (or (is-eq tx-sender (get proposer project)) (is-eq tx-sender (var-get admin))) ERR-NOT-PROJECT-OWNER)
        
        (map-set milestones {project-id: project-id, milestone-id: milestone-id} {
            id: milestone-id,
            title: title,
            description: description,
            funding-amount: funding-amount,
            status: MILESTONE-PENDING,
            due-date: due-date,
            completed-at: none,
            verification-notes: none
        })
        
        (map-set projects project-id (merge project {
            milestone-count: milestone-id,
            updated-at: stacks-block-height
        }))
        
        (ok milestone-id)
    )
)

;; Complete milestone
(define-public (complete-milestone (project-id uint) (milestone-id uint) (completion-notes (string-ascii 300)))
    (let (
        (project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
        (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) ERR-MILESTONE-NOT-FOUND))
    )
        ;; Only project owner can mark milestones complete
        (asserts! (is-eq tx-sender (get proposer project)) ERR-NOT-PROJECT-OWNER)
        
        (map-set milestones {project-id: project-id, milestone-id: milestone-id} (merge milestone {
            status: MILESTONE-COMPLETED,
            completed-at: (some stacks-block-height),
            verification-notes: (some completion-notes)
        }))
        
        (ok true)
    )
)

;; Verify and release milestone funding
(define-public (verify-milestone (project-id uint) (milestone-id uint) (approved bool))
    (let (
        (project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
        (milestone (unwrap! (map-get? milestones {project-id: project-id, milestone-id: milestone-id}) ERR-MILESTONE-NOT-FOUND))
        (funding-amount (get funding-amount milestone))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        
        (if approved
            (begin
                (map-set milestones {project-id: project-id, milestone-id: milestone-id} (merge milestone {
                    status: MILESTONE-VERIFIED
                }))
                
                (map-set projects project-id (merge project {
                    total-funding-released: (+ (get total-funding-released project) funding-amount),
                    updated-at: stacks-block-height
                }))
                
                (ok funding-amount)
            )
            (begin
                (map-set milestones {project-id: project-id, milestone-id: milestone-id} (merge milestone {
                    status: MILESTONE-PENDING
                }))
                
                (ok u0)
            )
        )
    )
)

;; Assign mentor to project
(define-public (assign-mentor (project-id uint) (mentor-principal principal) (expertise-area (string-ascii 50)))
    (let (
        (project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        (asserts! (is-none (map-get? mentors {project-id: project-id, mentor: mentor-principal})) ERR-ALREADY-MENTOR)
        
        (map-set mentors {project-id: project-id, mentor: mentor-principal} {
            expertise-area: expertise-area,
            assigned-at: stacks-block-height,
            active: true
        })
        
        (map-set projects project-id (merge project {
            mentor-count: (+ (get mentor-count project) u1),
            updated-at: stacks-block-height
        }))
        
        (ok true)
    )
)

;; Allocate community resources
(define-public (allocate-resource (project-id uint) (resource-type (string-ascii 50)) (amount uint) (notes (string-ascii 200)))
    (let (
        (project (unwrap! (map-get? projects project-id) ERR-PROJECT-NOT-FOUND))
        (resource (default-to {total-available: u0, allocated: u0, resource-type: resource-type, description: ""} 
                              (map-get? community-resources resource-type)))
    )
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        (asserts! (<= (+ (get allocated resource) amount) (get total-available resource)) ERR-INSUFFICIENT-FUNDS)
        
        (map-set resource-allocations {project-id: project-id, resource-type: resource-type} {
            amount: amount,
            allocated-at: stacks-block-height,
            notes: notes
        })
        
        (map-set community-resources resource-type (merge resource {
            allocated: (+ (get allocated resource) amount)
        }))
        
        (ok true)
    )
)

;; Add funds to community pool
(define-public (add-community-funds (amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
        (var-set total-community-funds (+ (var-get total-community-funds) amount))
        (ok (var-get total-community-funds))
    )
)

;; Read-only functions
(define-read-only (get-project (project-id uint))
    (map-get? projects project-id)
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
    (map-get? milestones {project-id: project-id, milestone-id: milestone-id})
)

(define-read-only (get-mentor-assignment (project-id uint) (mentor principal))
    (map-get? mentors {project-id: project-id, mentor: mentor})
)

(define-read-only (get-resource-allocation (project-id uint) (resource-type (string-ascii 50)))
    (map-get? resource-allocations {project-id: project-id, resource-type: resource-type})
)

(define-read-only (get-community-resource (resource-type (string-ascii 50)))
    (map-get? community-resources resource-type)
)

(define-read-only (get-project-count)
    (var-get project-counter)
)

(define-read-only (get-total-community-funds)
    (var-get total-community-funds)
)

(define-read-only (get-admin)
    (var-get admin)
)
