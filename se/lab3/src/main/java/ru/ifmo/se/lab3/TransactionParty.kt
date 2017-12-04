package ru.ifmo.se.lab3

interface TransactionParty {
  fun getTransactionParty(): Person
  fun getTransactionPartyLabel(): String
  fun getTransactionAccount(): BankAccount
}
