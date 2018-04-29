-- 1.

select Н_ЛЮДИ.ОТЧЕСТВО, Н_СЕССИЯ.ЧЛВК_ИД from Н_ЛЮДИ
  left join Н_СЕССИЯ on ЧЛВК_ИД = Н_ЛЮДИ.ИД
  where Н_ЛЮДИ.ОТЧЕСТВО < 'Георгиевич' and Н_СЕССИЯ.ИД > 14 and Н_СЕССИЯ.ИД < 14;

-- 2.

select Н_ЛЮДИ.ИМЯ, Н_ВЕДОМОСТИ.ЧЛВК_ИД, Н_СЕССИЯ.ДАТА from Н_ЛЮДИ
  inner join Н_ВЕДОМОСТИ on Н_ВЕДОМОСТИ.ЧЛВК_ИД = Н_ЛЮДИ.ИД
  inner join Н_СЕССИЯ on Н_СЕССИЯ.ЧЛВК_ИД = Н_ЛЮДИ.ИД
  where Н_ЛЮДИ.ИД = 100012 and Н_ВЕДОМОСТИ.ИД > 1426978;

-- 3.

with recursive names as (
  (select ИМЯ from Н_ЛЮДИ order by ИМЯ limit 1)
  union
  (select (select ИМЯ from Н_ЛЮДИ where ИМЯ > ns.ИМЯ order by ИМЯ limit 1)
    from names ns)
)
select count(*) from names where ИМЯ is not null;

-- 4.

select ps.ИД from Н_ПЛАНЫ ps
  inner join Н_ГРУППЫ_ПЛАНОВ gs on gs.ПЛАН_ИД = ps.ИД
  inner join Н_ОТДЕЛЫ ds on ps.ОТД_ИД_ЗАКРЕПЛЕН_ЗА = ds.ИД and ds.КОРОТКОЕ_ИМЯ = 'ВТ'
  group by ps.ИД having count(gs) > 2;

-- 5.

with max_age as (
  (select max(extract(year from justify_interval(now() - ДАТА_РОЖДЕНИЯ))) from Н_УЧЕНИКИ
   inner join Н_ЛЮДИ on Н_ЛЮДИ.ИД = ЧЛВК_ИД
   where ГРУППА = '1101'
   group by ГРУППА)
)
select ГРУППА, avg(extract(year from justify_interval(now() - ДАТА_РОЖДЕНИЯ))) from Н_УЧЕНИКИ
inner join Н_ЛЮДИ on Н_ЛЮДИ.ИД = ЧЛВК_ИД
group by ГРУППА
having round(avg(extract(year from justify_interval(now() - ДАТА_РОЖДЕНИЯ)))) =
  (select * from max_age);

-- 6.

select ГРУППА, Н_УЧЕНИКИ.ИД, ФАМИЛИЯ, ИМЯ, ОТЧЕСТВО from Н_УЧЕНИКИ
inner join Н_ЛЮДИ on Н_ЛЮДИ.ИД = ЧЛВК_ИД
where КОНЕЦ < '2012-09-01' and ПРИЗНАК = 'отчисл'
  and ПЛАН_ИД in (
    select Н_ПЛАНЫ.ИД from Н_ПЛАНЫ
    inner join Н_НАПРАВЛЕНИЯ_СПЕЦИАЛ
      on Н_ПЛАНЫ.НАПС_ИД = Н_НАПРАВЛЕНИЯ_СПЕЦИАЛ.ИД
    inner join Н_НАПР_СПЕЦ
      on НС_ИД = Н_НАПР_СПЕЦ.ИД and Н_НАПР_СПЕЦ.КОД_НАПРСПЕЦ = '230101');

-- 7.

with students_by_year(id, year) as (
  select ЧЛВК_ИД, extract(year from ДАТА) from Н_ВЕДОМОСТИ
  where СОСТОЯНИЕ = 'актуальна'
  group by 1, 2
  having min(case ОЦЕНКА
    when 'зачет' then '5'
    when 'осв' then '5'
    when 'неявка' then '2'
    when 'незач' then '2'
    else ОЦЕНКА end) >= '5'
)
select count(*), students_by_year.year
from students_by_year
group by 2
order by 2;

-- Recursive Fibonacci

with recursive fib(nprev, n) as (
  (select 1, 1)
  union
  (select n, nprev + n from fib)
)
select n from fib
limit 20;