package ru.ifmo.se.lab3.exception

public class DuplicateInsertionException(val record: Any): Exception(
  "Attempting to inserting a duplicate record, $record")
