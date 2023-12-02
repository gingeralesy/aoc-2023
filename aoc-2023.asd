#|
This file is a part of aoc-2023
(c) 2023 Janne Pakarinen (gingeralesy@gmail.com)
Author: Janne Pakarinen <gingeralesy@gmail.com>
|#

(in-package #:cl-user)
(asdf:defsystem aoc-2023
  :version "0.0.0"
  :license "zlib"
  :author "Janne Pakarinen <gingeralesy@gmail.com>"
  :maintainer "Janne Pakarinen <gingeralesy@gmail.com>"
  :description "Advent of Code 2023 - https://adventofcode.com/2023"
  :serial T
  :components ((:file "package")
               (:file "util")
               (:file "day1")
               (:file "day2"))
  :depends-on (:asdf
               :alexandria
               :cl-ppcre
               :local-time))
