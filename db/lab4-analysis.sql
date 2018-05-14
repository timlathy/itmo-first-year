-- 1. (enable_seqscan = true)

explain analyze select Н_ЛЮДИ.ФАМИЛИЯ, Н_СЕССИЯ.ДАТА from Н_ЛЮДИ
  inner join Н_СЕССИЯ on Н_СЕССИЯ.ЧЛВК_ИД = Н_ЛЮДИ.ИД
  where Н_ЛЮДИ.ФАМИЛИЯ < 'Петров' and Н_СЕССИЯ.ЧЛВК_ИД < 106059;

Hash Join  (cost=205.03..348.31 rows=1514 width=24)
           (actual time=3.624..6.197 rows=1735 loops=1)
  Hash Cond: ("Н_СЕССИЯ"."ЧЛВК_ИД" = "Н_ЛЮДИ"."ИД")
  ->  Seq Scan on "Н_СЕССИЯ"  (cost=0.00..117.90 rows=2733 width=12)
                              (actual time=0.011..1.168 rows=2728 loops=1)
        Filter: ("ЧЛВК_ИД" < 106059)
        Rows Removed by Filter: 1024
  ->  Hash  (cost=163.97..163.97 rows=3284 width=20)
            (actual time=3.604..3.604 rows=3309 loops=1)
        Buckets: 4096  Batches: 1  Memory Usage: 207kB
        ->  Seq Scan on "Н_ЛЮДИ"  (cost=0.00..163.97 rows=3284 width=20)
                                  (actual time=0.008..2.380 rows=3309 loops=1)
              Filter: (("ФАМИЛИЯ")::text < 'Петров'::text)
              Rows Removed by Filter: 1809
Planning time: 0.290 ms
Execution time: 6.617 ms

-- 1. (enable_seqscan = false)

Hash Join  (cost=337.29..467.85 rows=1514 width=24)
           (actual time=9.536..16.213 rows=1735 loops=1)
  Hash Cond: ("Н_СЕССИЯ"."ЧЛВК_ИД" = "Н_ЛЮДИ"."ИД")
  ->  Bitmap Heap Scan on "Н_СЕССИЯ"  (cost=61.46..166.62 rows=2733 width=12)
                                      (actual time=0.531..2.998 rows=2728 loops=1)
        Recheck Cond: ("ЧЛВК_ИД" < 106059)
        Heap Blocks: exact=71
        ->  Bitmap Index Scan on "SYS_C003500_IFK"  (cost=0.00..60.78 rows=2733 width=0)
                                                    (actual time=0.498..0.498 rows=2728 loops=1)
              Index Cond: ("ЧЛВК_ИД" < 106059)
  ->  Hash  (cost=234.78..234.78 rows=3284 width=20)
            (actual time=8.980..8.980 rows=3309 loops=1)
        Buckets: 4096  Batches: 1  Memory Usage: 207kB
        ->  Bitmap Heap Scan on "Н_ЛЮДИ"  (cost=93.73..234.78 rows=3284 width=20)
                                          (actual time=2.120..5.627 rows=3309 loops=1)
              Recheck Cond: (("ФАМИЛИЯ")::text < 'Петров'::text)
              Heap Blocks: exact=100
              ->  Bitmap Index Scan on "ФАМ_ЛЮД"  (cost=0.00..92.91 rows=3284 width=0)
                                                  (actual time=2.085..2.085 rows=3309 loops=1)
                    Index Cond: (("ФАМИЛИЯ")::text < 'Петров'::text)
Planning time: 0.686 ms
Execution time: 17.431 ms

-- 2. (enable_bitmapscan = false)

Hash Join  (cost=1068.83..1213.92 rows=915 width=32)
           (actual time=30.089..32.800 rows=744 loops=1)
  Hash Cond: ("Н_ОБУЧЕНИЯ"."ЧЛВК_ИД" = "Н_ЛЮДИ"."ИД")
  ->  Seq Scan on "Н_ОБУЧЕНИЯ"  (cost=0.00..119.76 rows=4388 width=4)
                                (actual time=0.028..2.346 rows=4387 loops=1)
        Filter: (("НЗК")::text > '001000'::text)
        Rows Removed by Filter: 634
  ->  Hash  (cost=1055.91..1055.91 rows=1034 width=36)
            (actual time=28.809..28.809 rows=834 loops=1)
        Buckets: 2048  Batches: 1  Memory Usage: 75kB
        ->  Hash Join  (cost=207.47..1055.91 rows=1034 width=36)
                       (actual time=8.437..28.177 rows=834 loops=1)
              Hash Cond: ("Н_УЧЕНИКИ"."ЧЛВК_ИД" = "Н_ЛЮДИ"."ИД")
              ->  Seq Scan on "Н_УЧЕНИКИ"  (cost=0.00..832.39 rows=1521 width=12)
                                           (actual time=0.337..18.595 rows=1468 loops=1)
                    Filter: (("ГРУППА")::text < '1101'::text)
                    Rows Removed by Filter: 21843
              ->  Hash  (cost=163.97..163.97 rows=3480 width=24)
                        (actual time=8.054..8.054 rows=3473 loops=1)
                    Buckets: 4096  Batches: 1  Memory Usage: 232kB
                    ->  Seq Scan on "Н_ЛЮДИ"  (cost=0.00..163.97 rows=3480 width=24)
                                              (actual time=0.014..4.555 rows=3473 loops=1)
                          Filter: ("ИД" < 142095)
                          Rows Removed by Filter: 1645
Planning time: 1.642 ms
Execution time: 33.054 ms

-- 2. (enable_bitmapscan = true, enable_seqscan = false)

Hash Join  (cost=883.59..1228.83 rows=915 width=32)
           (actual time=5.431..10.628 rows=744 loops=1)
  Hash Cond: ("Н_ОБУЧЕНИЯ"."ЧЛВК_ИД" = "Н_ЛЮДИ"."ИД")
  ->  Index Scan using "ОБУЧ_ЧЛВК_FK_I" on "Н_ОБУЧЕНИЯ"  (cost=0.28..320.19 rows=4388 width=4)
                                                         (actual time=0.011..3.585 rows=4387 loops=1)
        Filter: (("НЗК")::text > '001000'::text)
        Rows Removed by Filter: 634
  ->  Hash  (cost=870.38..870.38 rows=1034 width=36)
            (actual time=5.403..5.403 rows=834 loops=1)
        Buckets: 2048  Batches: 1  Memory Usage: 75kB
        ->  Hash Join  (cost=294.33..870.38 rows=1034 width=36)
                       (actual time=3.619..5.068 rows=834 loops=1)
              Hash Cond: ("Н_УЧЕНИКИ"."ЧЛВК_ИД" = "Н_ЛЮДИ"."ИД")
              ->  Bitmap Heap Scan on "Н_УЧЕНИКИ"  (cost=36.08..596.09 rows=1521 width=12)
                                                   (actual time=0.505..1.177 rows=1468 loops=1)
                    Recheck Cond: (("ГРУППА")::text < '1101'::text)
                    Heap Blocks: exact=184
                    ->  Bitmap Index Scan on "УЧЕН_ГП_FK_I"  (cost=0.00..35.70 rows=1521 width=0)
                                                             (actual time=0.486..0.486 rows=1468 loops=1)
                          Index Cond: (("ГРУППА")::text < '1101'::text)
              ->  Hash  (cost=214.75..214.75 rows=3480 width=24)
                        (actual time=3.106..3.106 rows=3473 loops=1)
                    Buckets: 4096  Batches: 1  Memory Usage: 232kB
                    ->  Bitmap Heap Scan on "Н_ЛЮДИ"  (cost=71.25..214.75 rows=3480 width=24)
                                                      (actual time=0.381..1.773 rows=3473 loops=1)
                          Recheck Cond: ("ИД" < 142095)
                          Heap Blocks: exact=97
                          ->  Bitmap Index Scan on "ЧЛВК_PK"  (cost=0.00..70.38 rows=3480 width=0)
                                                              (actual time=0.368..0.368 rows=3473 loops=1)
                                Index Cond: ("ИД" < 142095)
Planning time: 0.668 ms
Execution time: 10.857 ms
