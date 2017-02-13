#!/bin/python

import sys
import collections as cl
import math

import numpy as np
import scipy
import pandas as pd

####################

known_csv_names = ["incomplete_sets-2016_08_30.csv", "incomplete_sets-2016_08_02.csv"]
known_data_dir="./data"

special_header_ids =set(["pos","tgt","act","no","item","name","repair"])

class ExtNumSummary():

  def __init__(self, min_v, q1, median, q3, max_v, mean, stddev):
    self.min_v=min_v
    self.q1=q1
    self.median=median
    self.q3=q3
    self.max_v=max_v
    self.mean=mean
    self.stddev=stddev

  def iqr(self):
    return (self.q1, self.median, self.q3)

  def range_v(self):
    return (self.min_v, self.max_v)

  def mean_stddev(self):
    return (self.mean, self.stddev)

  def center(self):
    return (self.mean, self.median)

  def __str__(self):
    return "(%.0f, %.0f, %.0f, %.0f, %.0f) about %.4f +/- %.4f" % \
    (self.min_v, self.q1, self.median, self.q3, self.max_v, self.mean, self.stddev)

####################

def s_line(line):
  return line.strip().split(",")

def load(filepath):
  return [s_line(x) for x in open(filepath)]

def row_len_stats(numbers):
  if numbers is None or len(numbers) == 0:
    raise Exception("numbers is either None or has empty length!")

  numbers.sort()
  middle_index = len(numbers)/2

  min_v = numbers[0]
  max_v = numbers[-1]
  
  mean = sum(numbers) / float(len(numbers))
  variance = 0.0
  for x in numbers:
    variance += (x - mean) ** 2

  return ExtNumSummary(
    min_v=min_v,
    q1=numbers[middle_index/2],
    median=numbers[middle_index],
    q3=numbers[3 * (middle_index/2)],
    max_v=max_v,
    mean=mean,
    stddev=math.sqrt(variance)
  )

def header_row(lines):
  for row in lines:
    s_row = set([x.lower() for x in row])
    if "pos" in s_row or "act" in s_row or "tgt" in s_row:
      return row
  return []

def column_header_to_index(h_row):
  return dict(filter(lambda (x,i): x in special_header_ids, zip(h_row, xrange(len(h_row)))))

####################

if __name__=="__main__":

  for n in known_csv_names:

    name = "%s/%s" % (known_data_dir, n)
    lines = load(name)

    h_row = header_row(lines)
    print "Header row length: %d , content:\n%s\n" % (len(h_row), ",".join(h_row))
    print column_header_to_index(h_row)

    print "Five number summary + mean & std. deviation on %s" % name

    row_lengths = [len(x) for x in lines]

    stats = row_len_stats(row_lengths)

    bag={}
    bag["in_meidan"] = 0
    bag["outside_of"] = 0

    for x in row_lengths:
      if x == stats.median:
        bag["in_meidan"] += 1
      else:
        bag["outside_of"] += 1     


    print stats
    
    print ""

    i = -1
    o, looking_for = 0, bag["outside_of"]

    print bag
    print "Offending %d line(s):" % looking_for
    print ""

    for l in lines:
      i += 1
      if len(l) != stats.median:
        o += 1
        print "%d/%d | line index %d / %d | %s" % (o, looking_for, i, len(lines), ",".join(l))
        print ""

    print ""