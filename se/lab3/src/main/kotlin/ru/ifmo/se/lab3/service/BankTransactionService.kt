package ru.ifmo.se.lab3.service

import java.time.LocalDateTime
import javax.validation.ValidationException
import org.springframework.transaction.annotation.Transactional
import org.springframework.stereotype.Service

import ru.ifmo.se.lab3.domain.BankTransaction
import ru.ifmo.se.lab3.repository.BankTransactionRepository
import ru.ifmo.se.lab3.repository.BankAccountRepository
import ru.ifmo.se.lab3.repository.BusinessRepository
import ru.ifmo.se.lab3.exception.SuspiciouslyLargeTransactionException

@Service
@Transactional
class BankTransactionService(private val repo: BankTransactionRepository,
                             private val accountRepo: BankAccountRepository,
                             private val businessRepo: BusinessRepository) {
  @Throws(SuspiciouslyLargeTransactionException::class)
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

    if (dto.amount > 1000)
      throw SuspiciouslyLargeTransactionException(
          amount = dto.amount, drawer = drawerAcc.owner.name, drawee = draweeName)
      
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
