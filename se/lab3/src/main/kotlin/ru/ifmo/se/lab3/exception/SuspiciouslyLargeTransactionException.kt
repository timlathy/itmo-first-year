package ru.ifmo.se.lab3.exception

public class SuspiciouslyLargeTransactionException(
    val amount: Int,

    val drawee: String,

    val drawer: String): Exception("A transaction of a suspiciously large sum, $amount, has been requested between $drawee and $drawer.")
