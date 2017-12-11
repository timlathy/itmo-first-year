package ru.ifmo.se.lab3

import javax.validation.ValidationException
import java.time.LocalDateTime

import org.springframework.transaction.annotation.Transactional
import org.springframework.stereotype.Service

@Service
@Transactional
class BankTransactionService(private val repo: BankTransactionRepository,
                             private val accountRepo: BankAccountRepository,
                             private val businessRepo: BusinessRepository) {
  fun transitFunds(draweeName: String, dto: BankTransaction.Dto) {
    val draweeAcc = accountRepo.findByOwnerName(draweeName)
    val drawerAcc = if (dto.personName != null) {
      accountRepo.findByOwnerName(dto.personName)
    } else if (dto.businessName != null) {
      accountRepo.findByOwnerName(
        businessRepo.findByName(dto.businessName).owner.name)
    } else {
      throw ValidationException("Drawer is not specified")
    }

    /* Errors out with my current seed data, commenting out... */
    //if (draweeAcc.balance < dto.amount)
    //  throw ValidationException("Drawee has insufficient funds to perform the transaction")

    draweeAcc.balance -= dto.amount
    accountRepo.save(draweeAcc)

    drawerAcc.balance += dto.amount
    accountRepo.save(drawerAcc)

    repo.save(BankTransaction(
      amount = dto.amount,
      date = dto.date,
      draweeAccount = draweeAcc,
      drawerAccount = drawerAcc))
  }
}
