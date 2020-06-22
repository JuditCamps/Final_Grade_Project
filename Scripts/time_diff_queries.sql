CREATE DATABASE Block_times;
USE Block_times;

-- Import table original_times

SHOW FIELDS FROM original_times;

-- Change date_modification and time_modification to date and time:
CREATE TABLE all_times AS SELECT id_user, id_block,
	str_to_date(date_modification, '%d/%m/%Y') as date_modification, time_modification
	FROM original_times;

ALTER TABLE all_times
MODIFY COLUMN time_modification TIME;

-- Join date and time columns:
CREATE TABLE datetimes_all AS SELECT id_user, id_block,
	CAST(concat(date_modification, ' ', time_modification) as datetime) as date_time
FROM all_times; 


SELECT * FROM datetimes_all;

-- CREATE TABLES FOR PERSONALITY AND VITAL PLAN:
-- Select block from vital plan (14,15,16,17):
CREATE TABLE times_vital_plan AS SELECT * FROM datetimes_all
WHERE id_block in (14,15,16,17)
ORDER BY id_user;

-- Select block from personality (112,113,114,115,116):
CREATE TABLE times_personality AS SELECT * FROM datetimes_all
WHERE id_block in (112,113,114,115,116)
ORDER BY id_user;


-- PERSONALITY:
SELECT * FROM times_personality;

CREATE TABLE entries_personality AS SELECT a.id_user,
	a.first_entry, b.last_entry
    FROM
    (
		SELECT id_user, date_time as first_entry
        FROM times_personality
        WHERE id_block = 112
	) as a
    join
    (
		SELECT id_user, date_time as last_entry
		FROM times_personality
		WHERE id_block = 116
	) AS b ON a.id_user = b.id_user;

-- For id_users with more than one entry:
SELECT id_user, count(id_user)
FROM entries_personality
GROUP BY id_user
HAVING count(id_user)>1;

-- We choose the first entry to 112 as the first and the last of 116 as the last:
CREATE TABLE A_personality AS
SELECT t1.id_user, t2.first_entry, t2.last_entry
FROM (
	SELECT id_user, min(first_entry) as first_entry, max(last_entry) as last_entry
    FROM entries_personality
    GROUP BY id_user
    ) as t2
JOIN entries_personality as t1 ON t1.id_user = t2.id_user
GROUP BY id_user;


-- NOW FOR VITAL PLAN:
SELECT * FROM times_vital_plan;

CREATE TABLE entries_vital_plan AS SELECT a.id_user,
	a.first_entry, b.last_entry
    FROM
    (
		SELECT id_user, date_time as first_entry
        FROM times_vital_plan
        WHERE id_block = 14
	) as a
    join
    (
		SELECT id_user, date_time as last_entry
		FROM times_vital_plan
		WHERE id_block = 17
	) AS b ON a.id_user = b.id_user;

-- For id_users with more than one entry:
SELECT id_user, count(id_user)
FROM entries_vital_plan
GROUP BY id_user
HAVING count(id_user)>1;

-- We choose the first entry to 14 as the first and the last of 17 as the last:
CREATE TABLE A_vital_plan AS
SELECT t1.id_user, t2.first_entry, t2.last_entry
FROM (
	SELECT id_user, min(first_entry) as first_entry, max(last_entry) as last_entry
    FROM entries_vital_plan
    GROUP BY id_user
    ) as t2
JOIN entries_vital_plan as t1 ON t1.id_user = t2.id_user
GROUP BY id_user;


-- CALCULATE TIME DIFFERENCE BETWEEN FIRST AND LAST ENTRIES:
-- PERSONALITY:
CREATE TABLE personality_time_diff AS
SELECT id_user, TIMESTAMPDIFF(SECOND, first_entry, last_entry) as sec_difference
FROM A_personality
ORDER BY sec_difference;

-- VITAL PLAN:
CREATE TABLE vital_plan_time_diff AS
SELECT id_user, TIMESTAMPDIFF(SECOND, first_entry, last_entry) as sec_difference
FROM A_vital_plan
ORDER BY sec_difference;


/*
-- SHOW LAST 10 LINES OF EACH:

-- PERSONALITY:
SELECT * FROM (
    SELECT * FROM personality_time_diff ORDER BY sec_difference DESC LIMIT 40
) sub
ORDER BY sec_difference ASC;

SELECT * FROM (
    SELECT * FROM vital_plan_time_diff ORDER BY sec_difference DESC LIMIT 60
) sub
ORDER BY sec_difference ASC;
*/

