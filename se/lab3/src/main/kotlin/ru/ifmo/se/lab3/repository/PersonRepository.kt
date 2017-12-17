package ru.ifmo.se.lab3.repository

import org.springframework.data.repository.CrudRepository

import ru.ifmo.se.lab3.domain.Person

interface PersonRepository : CrudRepository<Person, Long> {
  fun findByName(name: String): Person
}
