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

;; Verifies if the specified user owns the document
(define-private (is-document-owner (doc-id uint) (user principal))
  (match (map-get? estate-documents { doc-id: doc-id })
    doc-details (is-eq (get owner doc-details) user)
    false
  )
)

;; Checks if document exists in registry
(define-private (document-exists (doc-id uint))
  (is-some (map-get? estate-documents { doc-id: doc-id }))
)

;; =========================================
;; DOCUMENT MANAGEMENT FUNCTIONS
;; =========================================

;; Creates a new real estate document in the registry
(define-public (register-document
  (title (string-ascii 64))
  (filesize uint)
  (description (string-ascii 128))
  (tags (list 10 (string-ascii 32)))
)
  (let
    (
      (next-id (+ (var-get document-counter) u1))
    )
    ;; Input validation checks
    (asserts! (> (len title) u0) title-format-error)
    (asserts! (< (len title) u65) title-format-error)
    (asserts! (> filesize u0) filesize-limit-error)
    (asserts! (< filesize u1000000000) filesize-limit-error)
    (asserts! (> (len description) u0) title-format-error)
    (asserts! (< (len description) u129) title-format-error)
    (asserts! (validate-tag-collection tags) tag-validation-error)

    ;; Create new document entry
    (map-insert estate-documents
      { doc-id: next-id }
      {
        title: title,
        owner: tx-sender,
        filesize: filesize,
        registration-block: block-height,
        description: description,
        tags: tags
      }
    )

    ;; Grant document access to creator
    (map-insert viewer-permissions
      { doc-id: next-id, viewer: tx-sender }
      { can-view: true }
    )

    ;; Update registry counter
    (var-set document-counter next-id)
    (ok next-id)
  )
)

;; Updates an existing document with new information
(define-public (update-document
  (doc-id uint)
  (new-title (string-ascii 64))
  (new-filesize uint)
  (new-description (string-ascii 128))
  (new-tags (list 10 (string-ascii 32)))
)
  (let
    (
      (doc-details (unwrap! (map-get? estate-documents { doc-id: doc-id })
        doc-not-found-error))
    )
    ;; Verify document exists and user has ownership rights
    (asserts! (document-exists doc-id) doc-not-found-error)
    (asserts! (is-eq (get owner doc-details) tx-sender) not-document-owner-error)

    ;; Validate all input fields
    (asserts! (> (len new-title) u0) title-format-error)
    (asserts! (< (len new-title) u65) title-format-error)
    (asserts! (> new-filesize u0) filesize-limit-error)
    (asserts! (< new-filesize u1000000000) filesize-limit-error)
    (asserts! (> (len new-description) u0) title-format-error)
    (asserts! (< (len new-description) u129) title-format-error)
    (asserts! (validate-tag-collection new-tags) tag-validation-error)

    ;; Update document with revised information
    (map-set estate-documents
      { doc-id: doc-id }
      (merge doc-details {
        title: new-title,
        filesize: new-filesize,
        description: new-description,
        tags: new-tags
      })
    )
    (ok true)
  )
)

;; Transfers document ownership to a different principal
(define-public (transfer-document-ownership (doc-id uint) (new-owner principal))
  (let
    (
      (doc-details (unwrap! (map-get? estate-documents { doc-id: doc-id })
        doc-not-found-error))
    )
    ;; Verify document exists and caller is current owner
    (asserts! (document-exists doc-id) doc-not-found-error)
    (asserts! (is-eq (get owner doc-details) tx-sender) not-document-owner-error)

    ;; Update ownership in document registry
    (map-set estate-documents
      { doc-id: doc-id }
      (merge doc-details { owner: new-owner })
    )
    (ok true)
  )
)

;; Removes a document completely from the registry
(define-public (delete-document (doc-id uint))
  (let
    (
      (doc-details (unwrap! (map-get? estate-documents { doc-id: doc-id })
        doc-not-found-error))
    )
    ;; Verify document exists and caller has ownership rights
    (asserts! (document-exists doc-id) doc-not-found-error)
    (asserts! (is-eq (get owner doc-details) tx-sender) not-document-owner-error)

    ;; Remove document from registry
    (map-delete estate-documents { doc-id: doc-id })
    (ok true)
  )
)


