#lang racket

#| STRUCTS FOR PARAGRAPHS, SENTENCES, WORDS, AND conjuclentionS |#

#| Paragraph Struct |#

;;; (paragraph sentences) -> paragraph?
;;;   sentences: list? of sentences?
;;; Struct for storing information about each paragraph




#| Sentence Struct |#

;;; (sentence words) -> sentence?
;;;   words : list? of word?
;;; Struct for storing information about each sentence




#| Word Struct |#

;;; (word str conjuclention) -> word?
;;;   str : string?
;;;   conjuclention : conjuclention?
;;; Creates a word struct




#| Conjuclention Struct |#

;;; (conjuclention part-of-speech conjucline) -> conjuclention?
;;;   part-of-speech : one of:
;;;       'noun, 'verb, 'adjective, 'adverb, 'preposition, 'conjunction, 'interjection, or #f
;;;       (pronouns count as nouns)
;;;   conjucline : declention? or conjugation? or #f
;;; Stores information about a words part of speech and conjugation or declention




#| Declention Struct |#

;;; (declention number gender case) -> declention?
;;;   number : 'singular or 'plural
;;;   gender : 'masculine, 'feminine, or 'neuter (default is 'neuter)
;;;   case : 'nominative, 'objective, or 'possessive (default is 'nominative)
;;; Stores information about a noun's (or pronoun's) declention
;;; Note:
;;;   * Nominative case is for subjects and complements
;;;     - "She ate the cake."
;;;     - "The person who ate the cake is she."
;;;   * Objective is used direct objects, indirect objects, and objects of preposition
;;;     - "I bought them."
;;;     - "I bought them a cake."
;;;     - "I bought a cake for them."
;;;   * Possessive case is used for possession
;;;     - "That's his cake."
;;;     - "That cake is his."




#| Conjugation Struct |#

;;; (conjugation person number tense) -> conjugation
;;;   person : 'first, 'second, or 'third
;;;   number : 'singular or 'plural
;;;   tense : 'past or 'present
;;; Stores information about a verb's conjugation




#| IDENTIFYING PARAGRAPHS, SENTENCES, AND WORDS |#

;;; (string->conjuclention str) -> conjuclention?
;;;   str : string?
;;; Analyses the word and returns the appropriate conjuclention struct
;;; NOTE: we won't actually analyze the word until part two.  For now, just
;;;  make it give a default value, such as clasifying all words as singular,
;;;  neuter, nominative nouns.
;;; TODO: Make this actually word (during part two)
(define string->conjuclention
  (lambda (str)
    (declention 'singular 'neuter 'nominative))) 

;;; (string->word str) -> word?
;;;    str : string?
;;; Converts a string to a word struct.
;;;  Assumes str contains one word
(define string->word
  (lambda (str)
    (word str
          (string->conjuclention str))))

;;; (string->sentence str) -> sentence?
;;;   str : string?
;;; Converts a string into a sentence struct.
;;;  Assumes str contains one sentence

;;; (string->paragraph str) -> paragraph?
;;;   str : string?
;;; Converts a string to a paragraph struct.
;;;  Assumes str contains only one paragraph.

;;; (file->paragraph filename) -> paragraph?
;;;   filename : string? that is a valid text file name
;;; Converts the text in a document into a paragraph struct.
;;;  Assumes str contains only one paragraph.

;;; (string->paragraphs str) -> list? of paragraph?
;;;   str : string?
;;; Creates a list of paragraphs contained within the string

;;; (file->paragraphs filename) -> list? of paragraph?
;;;   filename : string? that is a valid text file name
;;; Creates a list of paragraphs contained within the given text document

#| WORD ANALYSIS |#

#| OPTIONAL: SENTENCE ANALYSIS |#
