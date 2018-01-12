package ru.ifmo.se.lab3.web

import java.security.Principal;
import javax.validation.Valid;
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

import org.slf4j.LoggerFactory

import ru.ifmo.se.lab3.domain.BankTransaction
import ru.ifmo.se.lab3.repository.BankTransactionRepository
import ru.ifmo.se.lab3.service.BankTransactionService
import ru.ifmo.se.lab3.exception.SuspiciouslyLargeTransactionException

@RestController
class BankTransactionController(private val repo: BankTransactionRepository,
                                private val transactionService: BankTransactionService) {
  private val logger = LoggerFactory.getLogger("BANK TRANSACTIONS")

  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @GetMapping("/transactions/drawee/{personName}")
  fun findByDrawee(@PathVariable personName: String) =
    repo.findByDraweeAccountOwnerName(personName)
  
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @GetMapping("/transactions/drawer/{personName}")
  fun findByDrawer(@PathVariable personName: String) =
    repo.findByDrawerAccountOwnerName(personName)

  @PreAuthorize("hasRole('ROLE_SURV_DEVICE')")
  @PostMapping("/transactions")
  fun createTransaction(authorizedDrawee: Principal,
                        @Valid @RequestBody transaction: BankTransaction.Dto): Map<String, String> {
    try {
      transactionService.transitFunds(authorizedDrawee.getName(), transaction)
      return mapOf()
    }
    catch (s: SuspiciouslyLargeTransactionException) {
      logger.info(s.message)
      return mapOf("error" to "The amount requested exceeds transaction limits. Please have the transaction approved by your nearest bank office.")
    }
  }
}
