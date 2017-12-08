package ru.ifmo.se.lab3

import javax.validation.ValidationException
import java.time.LocalDateTime

import org.springframework.transaction.annotation.Transactional
import org.springframework.stereotype.Service

@Service
@Transactional
class BankTransactionService(private val repo: BankTransactionRepository,
                             private val accountRepo: BankAccountRepository) {
  fun transitFunds(draweeName: String, dto: BankTransaction.Dto) {
    val draweeAcc = accountRepo.findByOwnerName(draweeName)
    val drawerAcc = accountRepo.findByOwnerName(dto.drawerName)

    if (draweeAcc.balance < dto.amount)
      throw ValidationException("Drawee has insufficient funds to perform the transaction")

    draweeAcc.balance -= dto.amount
    accountRepo.save(draweeAcc)

    drawerAcc.balance += dto.amount
    accountRepo.save(drawerAcc)

    repo.save(BankTransaction(
      amount = dto.amount,
      date = LocalDateTime.now(),
      draweeAccount = draweeAcc,
      drawerAccount = drawerAcc))
  }
}
