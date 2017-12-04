package ru.ifmo.se.lab3

import org.springframework.transaction.annotation.Transactional
import org.springframework.stereotype.Service

@Service
@Transactional
class BankTransactionService(private val repo: BankTransactionRepository,
                             private val accountRepo: BankAccountRepository) {
  fun transitFunds(amount: Int, drawee: TransactionParty, drawer: TransactionParty) {
    /* TODO: make sure the drawee has enough funds to withdraw */
    val draweeAcc = drawee.getTransactionAccount()
    draweeAcc.balance -= amount
    accountRepo.save(draweeAcc)

    val drawerAcc = drawer.getTransactionAccount()
    drawerAcc.balance += amount
    accountRepo.save(drawerAcc)

    val transaction = BankTransaction(amount,
      drawee = drawee.getTransactionParty(),
      draweeLabel = drawee.getTransactionPartyLabel(),
      drawer = drawer.getTransactionParty(),
      drawerLabel = drawer.getTransactionPartyLabel())
    repo.save(transaction)
  }
}
