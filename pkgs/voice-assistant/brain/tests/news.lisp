;;; News, ported from the Rust tests.

(in-package #:va-tests)

(define-test news-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "News(~a)"
               '(("What's the news?" "World") ("Read me the headlines" "World")
                 ("Headlines please." "World") ("What's on Hacker News?" "Tech")
                 ("Tech news" "Tech")))))

(define-test news-feeds
  :parent va-tests
  (is equal '("Storm hits & floods" "Second")
      (va::headlines (concatenate 'string "<rss><channel><title>BBC News</title><item><title>"
                                  "<![CDATA[Storm hits &amp; floods]]></title></item>"
                                  "<item><title>Watch: wind</title></item>"
                                  "<item><title>Second</title></item></channel></rss>")))
  (is equal '("Rust 2.0 \"soon\"")
      (va::headlines (concatenate 'string "<feed><title>feed</title><entry><title type=\"html\">"
                                  "Rust 2.0 &quot;soon&quot;</title></entry></feed>")))
  (false (va::headlines "garbage")))
