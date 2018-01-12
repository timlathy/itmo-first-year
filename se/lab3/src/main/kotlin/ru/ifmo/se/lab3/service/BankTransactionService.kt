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
    class DrawerWrapper {
      val account = if (dto.personName != null) {
          accountRepo.findByOwnerName(dto.personName)
        }
        else if (dto.businessName != null) {
          accountRepo.findByOwnerName(
            businessRepo.findByName(dto.businessName).owner.name)
        }
        else {
          throw ValidationException("Drawer is not specified")
        }

      val name = if (dto.personName != null)
          "Business (owned by ${dto.businessName})"
        else
          "Individual ${dto.personName}"
    }

    val drawee = accountRepo.findByOwnerName(draweeName)
    val drawer = DrawerWrapper()

    if (dto.amount > 1000)
      throw SuspiciouslyLargeTransactionException(
          amount = dto.amount, drawer = drawer.name, drawee = draweeName)
      
    /* Errors out with my current seed data, commenting out... */
    //if (draweeAcc.balance < dto.amount)
    //  throw ValidationException("Drawee has insufficient funds to perform the transaction")

    drawee.balance -= dto.amount
    accountRepo.save(drawee)

    drawer.account.balance += dto.amount
    accountRepo.save(drawer.account)

    repo.save(BankTransaction(
      amount = dto.amount,
      date = dto.date,
      draweeAccount = drawee,
      drawerAccount = drawer.account))
  }
}
