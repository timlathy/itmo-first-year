package ru.ifmo.se.lab3.web

import java.security.Principal;
import javax.validation.Valid;
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

import ru.ifmo.se.lab3.domain.BankTransaction
import ru.ifmo.se.lab3.repository.BankTransactionRepository
import ru.ifmo.se.lab3.service.BankTransactionService

@RestController
class BankTransactionController(private val repo: BankTransactionRepository,
                                private val transactionService: BankTransactionService) {
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
                        @Valid @RequestBody transaction: BankTransaction.Dto) =
    transactionService.transitFunds(authorizedDrawee.getName(), transaction)
}
