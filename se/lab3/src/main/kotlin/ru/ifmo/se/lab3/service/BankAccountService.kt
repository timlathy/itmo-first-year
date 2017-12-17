package ru.ifmo.se.lab3.service

import org.springframework.stereotype.Service

import ru.ifmo.se.lab3.domain.BankAccount
import ru.ifmo.se.lab3.repository.BankAccountRepository
import ru.ifmo.se.lab3.repository.PersonRepository

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
