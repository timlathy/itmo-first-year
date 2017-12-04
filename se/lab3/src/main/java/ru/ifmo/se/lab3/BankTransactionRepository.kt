package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository
import org.springframework.data.jpa.repository.Query

interface BankTransactionRepository : CrudRepository<BankTransaction, Long> {
  fun findByDrawee(drawee: Person): Iterable<BankTransaction>
  fun findByDrawer(drawer: Person): Iterable<BankTransaction>
}
