package ru.ifmo.se.lab3

import java.security.Principal;
import javax.validation.Valid;
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

@RestController
class BankTransactionController(private val repo: BankTransactionRepository,
                                private val transactionService: BankTransactionService) {
  @GetMapping("/transactions/drawee/{personName}")
  fun findByDrawee(@PathVariable personName: String) =
    repo.findByDraweeAccountOwnerName(personName)
  
  @GetMapping("/transactions/drawer/{personName}")
  fun findByDrawer(@PathVariable personName: String) =
    repo.findByDrawerAccountOwnerName(personName)

  @PreAuthorize("hasRole('ROLE_DEVICE')")
  @PostMapping("/transactions")
  fun createTransaction(authorizedDrawee: Principal,
                        @Valid @RequestBody transaction: BankTransaction.Dto) =
    transactionService.transitFunds(authorizedDrawee.getName(), transaction)
}
