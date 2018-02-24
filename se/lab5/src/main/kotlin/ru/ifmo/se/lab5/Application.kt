package ru.ifmo.se.lab5

fun main(args: Array<String>) =
  Repl(CommandRunner(EmploymentRequestCommands())).loop()