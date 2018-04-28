set enable_indexscan = false;
set enable_bitmapscan = false;

explain analyze select Н_ЛЮДИ.ОТЧЕСТВО, Н_СЕССИЯ.ЧЛВК_ИД from Н_ЛЮДИ
  left join Н_СЕССИЯ on ЧЛВК_ИД = Н_ЛЮДИ.ИД
  where Н_ЛЮДИ.ОТЧЕСТВО < 'Георгиевич' and Н_СЕССИЯ.ИД > 14 and Н_СЕССИЯ.ИД < 14;

------------------------------------------------------------------------------------------------------------------
                                                    QUERY PLAN                                                    
------------------------------------------------------------------------------------------------------------------
 Hash Join  (cost=127.29..301.77 rows=1 width=24) (actual time=3.483..3.483 rows=0 loops=1)
   Hash Cond: ("Н_ЛЮДИ"."ИД" = "Н_СЕССИЯ"."ЧЛВК_ИД")
   ->  Seq Scan on "Н_ЛЮДИ"  (cost=0.00..163.97 rows=2798 width=24) (actual time=0.095..0.095 rows=1 loops=1)
         Filter: (("ОТЧЕСТВО")::text < 'Георгиевич'::text)
         Rows Removed by Filter: 1
   ->  Hash  (cost=127.28..127.28 rows=1 width=4) (actual time=3.245..3.245 rows=0 loops=1)
         Buckets: 1024  Batches: 1  Memory Usage: 8kB
         ->  Seq Scan on "Н_СЕССИЯ"  (cost=0.00..127.28 rows=1 width=4) (actual time=3.243..3.243 rows=0 loops=1)
               Filter: (("ИД" > 14) AND ("ИД" < 14))
               Rows Removed by Filter: 3752
 Planning time: 3.083 ms
 Execution time: 3.555 ms
------------------------------------------------------------------------------------------------------------------

set enable_indexscan = true;
set enable_bitmapscan = true;

explain select Н_ЛЮДИ.ОТЧЕСТВО, Н_СЕССИЯ.ЧЛВК_ИД from Н_ЛЮДИ
  left join Н_СЕССИЯ on ЧЛВК_ИД = Н_ЛЮДИ.ИД
  where Н_ЛЮДИ.ОТЧЕСТВО < 'Георгиевич' and Н_СЕССИЯ.ИД > 14 and Н_СЕССИЯ.ИД < 14;

------------------------------------------------------------------------------------------------------------
                                                 QUERY PLAN                                                 
------------------------------------------------------------------------------------------------------------
 Nested Loop  (cost=0.28..135.59 rows=1 width=24) (actual time=0.663..0.663 rows=0 loops=1)
   ->  Seq Scan on "Н_СЕССИЯ"  (cost=0.00..127.28 rows=1 width=4) (actual time=0.662..0.662 rows=0 loops=1)
         Filter: (("ИД" > 14) AND ("ИД" < 14))
         Rows Removed by Filter: 3752
   ->  Index Scan using "ЧЛВК_PK" on "Н_ЛЮДИ"  (cost=0.28..8.30 rows=1 width=24) (never executed)
         Index Cond: ("ИД" = "Н_СЕССИЯ"."ЧЛВК_ИД")
         Filter: (("ОТЧЕСТВО")::text < 'Георгиевич'::text)
 Planning time: 0.348 ms
 Execution time: 0.701 ms
------------------------------------------------------------------------------------------------------------

set enable_indexscan = false;
set enable_bitmapscan = false;

explain analyze select Н_ЛЮДИ.ИМЯ, Н_ВЕДОМОСТИ.ЧЛВК_ИД, Н_СЕССИЯ.ДАТА from Н_ЛЮДИ
  inner join Н_ВЕДОМОСТИ on Н_ВЕДОМОСТИ.ЧЛВК_ИД = Н_ЛЮДИ.ИД
  inner join Н_СЕССИЯ on Н_СЕССИЯ.ЧЛВК_ИД = Н_ЛЮДИ.ИД
  where Н_ЛЮДИ.ИД = 100012 and Н_ВЕДОМОСТИ.ИД > 1426978;

------------------------------------------------------------------------------------------------------------------------------
                                                          QUERY PLAN                                                          
------------------------------------------------------------------------------------------------------------------------------
 Nested Loop  (cost=0.00..7684.68 rows=14 width=25) (actual time=61.355..61.355 rows=0 loops=1)
   ->  Seq Scan on "Н_СЕССИЯ"  (cost=0.00..117.90 rows=7 width=12) (actual time=0.038..0.507 rows=6 loops=1)
         Filter: ("ЧЛВК_ИД" = 100012)
         Rows Removed by Filter: 3746
   ->  Materialize  (cost=0.00..7566.61 rows=2 width=21) (actual time=10.140..10.140 rows=0 loops=6)
         ->  Nested Loop  (cost=0.00..7566.60 rows=2 width=21) (actual time=60.836..60.836 rows=0 loops=1)
               ->  Seq Scan on "Н_ЛЮДИ"  (cost=0.00..163.97 rows=1 width=17) (actual time=0.953..1.951 rows=1 loops=1)
                     Filter: ("ИД" = 100012)
                     Rows Removed by Filter: 5117
               ->  Seq Scan on "Н_ВЕДОМОСТИ"  (cost=0.00..7402.60 rows=2 width=4) (actual time=58.883..58.883 rows=0 loops=1)
                     Filter: (("ИД" > 1426978) AND ("ЧЛВК_ИД" = 100012))
                     Rows Removed by Filter: 222440
 Planning time: 0.213 ms
 Execution time: 61.398 ms
------------------------------------------------------------------------------------------------------------------------------

set enable_indexscan = true;
set enable_bitmapscan = true;

explain analyze select Н_ЛЮДИ.ИМЯ, Н_ВЕДОМОСТИ.ЧЛВК_ИД, Н_СЕССИЯ.ДАТА from Н_ЛЮДИ
  inner join Н_ВЕДОМОСТИ on Н_ВЕДОМОСТИ.ЧЛВК_ИД = Н_ЛЮДИ.ИД
  inner join Н_СЕССИЯ on Н_СЕССИЯ.ЧЛВК_ИД = Н_ЛЮДИ.ИД
  where Н_ЛЮДИ.ИД = 100012  and Н_ВЕДОМОСТИ.ИД > 1426978;

---------------------------------------------------------------------------------------------------------------------------------------------------
                                                                    QUERY PLAN                                                                     
---------------------------------------------------------------------------------------------------------------------------------------------------
 Nested Loop  (cost=123.02..160.63 rows=14 width=25) (actual time=0.171..0.171 rows=0 loops=1)
   ->  Bitmap Heap Scan on "Н_СЕССИЯ"  (cost=4.33..25.83 rows=7 width=12) (actual time=0.043..0.113 rows=6 loops=1)
         Recheck Cond: ("ЧЛВК_ИД" = 100012)
         Heap Blocks: exact=5
         ->  Bitmap Index Scan on "SYS_C003500_IFK"  (cost=0.00..4.33 rows=7 width=0) (actual time=0.030..0.030 rows=6 loops=1)
               Index Cond: ("ЧЛВК_ИД" = 100012)
   ->  Materialize  (cost=118.69..134.63 rows=2 width=21) (actual time=0.007..0.007 rows=0 loops=6)
         ->  Nested Loop  (cost=118.69..134.62 rows=2 width=21) (actual time=0.035..0.035 rows=0 loops=1)
               ->  Index Scan using "ЧЛВК_PK" on "Н_ЛЮДИ"  (cost=0.28..8.30 rows=1 width=17) (actual time=0.012..0.013 rows=1 loops=1)
                     Index Cond: ("ИД" = 100012)
               ->  Bitmap Heap Scan on "Н_ВЕДОМОСТИ"  (cost=118.41..126.30 rows=2 width=4) (actual time=0.014..0.014 rows=0 loops=1)
                     Recheck Cond: (("ЧЛВК_ИД" = 100012) AND ("ИД" > 1426978))
                     ->  BitmapAnd  (cost=118.41..118.41 rows=2 width=0) (actual time=0.012..0.012 rows=0 loops=1)
                           ->  Bitmap Index Scan on "ВЕД_ЧЛВК_FK_IFK"  (cost=0.00..4.90 rows=64 width=0) (actual time=0.011..0.011 rows=0 loops=1)
                                 Index Cond: ("ЧЛВК_ИД" = 100012)
                           ->  Bitmap Index Scan on "ВЕД_PK"  (cost=0.00..113.26 rows=5978 width=0) (never executed)
                                 Index Cond: ("ИД" > 1426978)
 Planning time: 0.451 ms
 Execution time: 0.285 ms
---------------------------------------------------------------------------------------------------------------------------------------------------
