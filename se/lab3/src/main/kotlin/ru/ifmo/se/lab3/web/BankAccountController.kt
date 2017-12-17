package ru.ifmo.se.lab3.web

import javax.validation.Valid;
import org.springframework.web.bind.annotation.*
import org.springframework.security.access.prepost.PreAuthorize

import ru.ifmo.se.lab3.domain.BankAccount
import ru.ifmo.se.lab3.repository.BankAccountRepository
import ru.ifmo.se.lab3.service.BankAccountService

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
