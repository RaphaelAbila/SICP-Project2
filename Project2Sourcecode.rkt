#lang racket

(require csv-reading math math/matrix racket/hash)
(require data-science-master)
(require plot)

;; Step 1: Load and Clean Dataset
(define (load-twitter-data file-path)
  "Loads CSV data from the provided file path."
  (read-csv file-path #:header? #t))

(define (clean-tweet text)
  "Cleans a tweet by removing URLs, punctuation, and normalizing spaces."
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls
     (string-downcase text)))))

;; Step 2: Analyze Sentiment
(define (analyze-sentiment tweets)
  "Analyzes sentiment of tweets using NRC lexicon."
  (define cleaned-tweets (map clean-tweet tweets))
  (define all-clean (string-join cleaned-tweets " "))
  (define words (document->tokens all-clean #:sort? #t))
  (list->sentiment words #:lexicon 'nrc))

;; Step 3: Summarize and Visualize Sentiment
(define (summarize-sentiment sentiment)
  "Aggregates sentiment frequencies for visualization."
  (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq)))

(define (visualize-sentiment counts)
  "Visualizes sentiment frequencies as a histogram."
  (parameterize ((plot-width 800))
    (plot (list
           (tick-grid)
           (discrete-histogram
            (sort counts (λ (x y) (> (second x) (second y))))
            #:color "Crimson"            
            #:line-color "DarkSlateGray")) 
          #:x-label "Affective Label"
          #:y-label "Frequency")))

;; Step 4: Main Function to Tie Everything Together
(define (analyze-twitter-moods file-path)
  "Loads, cleans, and analyzes tweet data, then visualizes sentiment trends."
  (let* ([twitter-data (load-twitter-data file-path)]
         [created-at-column ($ twitter-data 'created_at)]
         [text-column ($ twitter-data 'text)]
         [sentiment (analyze-sentiment text-column)]
         [counts (summarize-sentiment sentiment)])
    (visualize-sentiment counts)))

;; Run analysis (provide the correct path to your dataset)
(analyze-twitter-moods "uganda.csv")

;; Step 4: Group Sentiment by Month
#|(define (group-by-month dataset sentiment-data)
  (define date-column (map (λ (row) (hash-ref row 'created_at)) dataset))
  (define grouped-data
    (for/fold ([monthly-data (make-hash)]) ([idx (in-range (length date-column))])
      (let ([date-str (list-ref date-column idx)]
            [sentiment (list-ref sentiment-data idx)])
        (define parsed-date (string->date date-str))
        (define month (format "~a-~a" (date-year parsed-date) (date-month parsed-date)))
        (hash-set! monthly-data month
                   (append (hash-ref monthly-data month '()) (list sentiment)))
        monthly-data)))
  grouped-data)|#