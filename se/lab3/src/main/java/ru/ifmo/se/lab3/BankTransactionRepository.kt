package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository
import org.springframework.data.jpa.repository.Query

interface BankTransactionRepository : CrudRepository<BankTransaction, Long> {
  fun findByDraweeAccountOwnerName(name: String): Iterable<BankTransaction>
  fun findByDrawerAccountOwnerName(name: String): Iterable<BankTransaction>
}
