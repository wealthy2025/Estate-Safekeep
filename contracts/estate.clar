;; Estate Safekeep
;; A decentralized system for maintaining real estate documentation and ownership records with secure access controls and comprehensive metadata management

;; Global Registry Counter
(define-data-var document-counter uint u0)

;; =========================================
;; SYSTEM ADMINISTRATOR CONFIGURATION
;; =========================================
;; System Administration Principal
(define-constant admin-principal tx-sender)

;; =========================================
;; ERROR CODE DEFINITIONS
;; =========================================
;; Administrative error codes
(define-constant admin-operation-denied (err u300))
;; Document-related error codes  
(define-constant doc-not-found-error (err u301))
(define-constant doc-already-exists-error (err u302))
(define-constant title-format-error (err u303))
(define-constant filesize-limit-error (err u304))
(define-constant permission-denied-error (err u305))
(define-constant not-document-owner-error (err u306))
(define-constant reading-restricted-error (err u307))
(define-constant tag-validation-error (err u308))

;; =========================================
;; CORE DATA STRUCTURES
;; =========================================
;; Primary document storage map
(define-map estate-documents
  { doc-id: uint }
  {
    title: (string-ascii 64),
    owner: principal,
    filesize: uint,
    registration-block: uint,
    description: (string-ascii 128),
    tags: (list 10 (string-ascii 32))
  }
)

;; Document permissions registry
(define-map viewer-permissions
  { doc-id: uint, viewer: principal }
  { can-view: bool }
)

;; =========================================
;; HELPER FUNCTIONS FOR VALIDATION
;; =========================================

;; Validates if all tags follow proper formatting rules
(define-private (validate-tag-collection (tags (list 10 (string-ascii 32))))
  (and
    (> (len tags) u0)
    (<= (len tags) u10)
    (is-eq (len (filter is-valid-tag tags)) (len tags))
  )
)

;; Checks if an individual tag meets format requirements
(define-private (is-valid-tag (tag (string-ascii 32)))
  (and
    (> (len tag) u0)
    (< (len tag) u33)
  )
)

;; Retrieves the filesize for a document
(define-private (get-document-size (doc-id uint))
  (default-to u0
    (get filesize
      (map-get? estate-documents { doc-id: doc-id })
    )
  )
)
