package ru.ifmo.se.lab3

import org.springframework.stereotype.Service

@Service
class BankAccountService(private val repo: BankAccountRepository,
                         private val personRepo: PersonRepository) {
  fun createAccount(dto: BankAccount.Dto) {
    val owner = personRepo.findByName(dto.ownerName)

    repo.save(BankAccount(
      balance = dto.balance,
      name = dto.name,
      owner = owner))
  }
}
