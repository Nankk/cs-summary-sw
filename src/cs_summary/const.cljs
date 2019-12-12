(ns cs-summary.const)

(def root js/__dirname)

(def sw-bind-table "console-char-table!")
(def sw-data-table "char-data-table!")
(def sw-var-table "char-var-table!") ; Not implemented yet
(def sw-sheet-id "1oMDM0k9TfPREXhXHIrTUPCpxmZpwmhq79yiCkt1JdMM")

(def coc-bind-table "console-bind-table!")
(def coc-data-table "char-data-table!")
(def coc-var-table "char-var-table!")
(def coc-sheet-id "1KSIxjRdT9kr3wUER9hnNjsYKP4sDeJ9gDi5goMNzv4U")

;; Rating table from 0 to 50; index corresponds to the weapon power
(def weapon-rating-table [[0 0 0 1 2 2 3 3 4 4]
                          [0 0 0 1 2 3 3 3 4 4]
                          [0 0 0 1 2 3 4 4 4 4]
                          [0 0 1 1 2 3 4 4 4 5]
                          [0 0 1 2 2 3 4 4 5 5]
                          [0 1 1 2 2 3 4 5 5 5]
                          [0 1 1 2 3 3 4 5 5 5]
                          [0 1 1 2 3 4 4 5 5 6]
                          [0 1 2 2 3 4 4 5 6 6]
                          [0 1 2 3 3 4 4 5 6 7]
                          [1 1 2 3 3 4 5 5 6 7]
                          [1 2 2 3 3 4 5 6 6 7]
                          [1 2 2 3 4 4 5 6 6 7]
                          [1 2 3 3 4 4 5 6 7 7]
                          [1 2 3 4 4 4 5 6 7 8]
                          [1 2 3 4 4 5 5 6 7 8]
                          [1 2 3 4 4 5 6 7 7 8]
                          [1 2 3 4 5 5 6 7 7 8]
                          [1 2 3 4 5 6 6 7 7 8]
                          [1 2 3 4 5 6 7 7 8 9]
                          [1 2 3 4 5 6 7 8 9 10]
                          [1 2 3 4 6 6 7 8 9 10]
                          [1 2 3 5 6 6 7 8 9 10]
                          [2 2 3 5 6 7 7 8 9 10]
                          [2 3 4 5 6 7 7 8 9 10]
                          [2 3 4 5 6 7 8 8 9 10]
                          [2 3 4 5 6 8 8 9 9 10]
                          [2 3 4 6 6 8 8 9 9 10]
                          [2 3 4 6 6 8 9 9 10 10]
                          [2 3 4 6 7 8 9 9 10 10]
                          [2 4 4 6 7 8 9 10 10 10]
                          [2 4 5 6 7 8 9 10 10 11]
                          [3 4 5 6 7 8 10 10 10 11]
                          [3 4 5 6 8 8 10 10 10 11]
                          [3 4 5 6 8 9 10 10 11 11]
                          [3 4 5 7 8 9 10 10 11 12]
                          [3 5 5 7 8 9 10 11 11 12]
                          [3 5 6 7 8 9 10 11 12 12]
                          [3 5 6 7 8 10 10 11 12 13]
                          [4 5 6 7 8 10 11 11 12 13]
                          [4 5 6 7 9 10 11 11 12 13]
                          [4 6 6 7 9 10 11 12 12 13]
                          [4 6 7 7 9 10 11 12 13 13]
                          [4 6 7 8 9 10 11 12 13 14]
                          [4 6 7 8 10 10 11 12 13 14]
                          [4 6 7 9 10 10 11 12 13 14]
                          [4 6 7 9 10 10 12 13 13 14]
                          [4 6 7 9 10 11 12 13 13 15]
                          [4 6 7 9 10 12 12 13 13 15]
                          [4 6 7 10 10 12 12 13 14 15]
                          [4 6 8 10 10 12 12 13 15 15]])
