package ru.ifmo.se.lab3

import javax.validation.Valid;
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

@RestController
class BankAccountController(private val repo: BankAccountRepository,
                            private val accountService: BankAccountService) {
  @PreAuthorize("hasRole('ROLE_BIG_BROTHER')")
  @PostMapping("/accounts")
  fun createAccount(@Valid @RequestBody dto: BankAccount.Dto) =
    accountService.createAccount(dto)

  @GetMapping("/accounts/{accountName}")
  fun findByName(@PathVariable accountName: String) =
    repo.findByName(accountName)
}
