create table all_beat (bar_id integer, beat_id integer);

create table song (
  song_id            integer not null,
  song_nm            varchar(50),
  time_sig_nmrtr_num integer,
  time_sig_denom_num integer,
  time_sig_ppq_num   integer,
  time_sig_bb_num    integer,
  bpm_num            integer,
  primary key(song_id)
);

create table bar (
  song_id integer not null,
  bar_id  integer not null,
  beat_id integer not null,
  chord_id varchar(20),
  primary key (song_id, bar_id, beat_id)
);

create table song_bar_beat (
  song_id integer not null,
  bar_id  integer not null,
  beat_id integer not null,
  chord_id varchar(20),
  primary key (song_id, bar_id, beat_id)
);

select a.bar_id, a.beat_id, b.chord_id
from all_beat      a
     left join bar b on (b.bar_id  = a.bar_id and
                         b.beat_id = a.beat_id)
where a.beat_id <= 4 and
      a.bar_id <= (select max(bar_id) max_bar from bar where song_id = 1) and
      b.song_id = 1
order by a.bar_id, a.beat_id;

with
song as (select * from bar_flat where song_id = 2),
fill as (
    select a.bar_id, a.beat_id, b.bar_id orig_bar_id, b.beat_id orig_beat_id,
           b.chord_id,
           lag(b.chord_id, 1) over (partition by a.bar_id order by a.beat_id) c1,
           lag(b.chord_id, 2) over (partition by a.bar_id order by a.beat_id) c2,
           lag(b.chord_id, 3) over (partition by a.bar_id order by a.beat_id) c3,
           lag(b.chord_id, 4) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c4,
           lag(b.chord_id, 5) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c5,
           lag(b.chord_id, 6) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c6,
           lag(b.chord_id, 7) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c7,
           lag(b.chord_id, 8) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c8
    from all_beat                 a
         left outer join bar_flat b on (a.bar_id = b.bar_id and a.beat_id = b.beat_id)
    where song_id = 2
     ),
rc as (select bar_id, beat_id, coalesce(chord_id, c1, c2, c3, c4, c5, c6, c7, c8) chord_id,
              orig_bar_id, orig_beat_id, chord_id orig_chord_id
       from fill)
select * from rc
-- where chord_id is not null
order by 1, 2;

with
bars as (select * from bar_flat where song_id = 6),
fill as (
    select a.bar_id, a.beat_id, b.bar_id orig_bar_id, b.beat_id orig_beat_id,
           b.chord_id,
           lag(b.chord_id, 1) over (partition by a.bar_id order by a.beat_id) c1,
           lag(b.chord_id, 2) over (partition by a.bar_id order by a.beat_id) c2,
           lag(b.chord_id, 3) over (partition by a.bar_id order by a.beat_id) c3,
           lag(b.chord_id, 4) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c4,
           lag(b.chord_id, 5) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c5,
           lag(b.chord_id, 6) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c6,
           lag(b.chord_id, 7) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c7,
           lag(b.chord_id, 8) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c8
    from all_beat             a
         left outer join bars b on (a.bar_id = b.bar_id and a.beat_id = b.beat_id)),
rc as (select bar_id, beat_id, coalesce(chord_id, c1, c2, c3, c4, c5, c6, c7, c8) chord_id
       from fill)
select * from rc
where chord_id is not null
order by 1, 2;


select * from bar where song_id = 5;

with
bars as (select * from bar_flat where upper(song_nm) = 'ALL THE THINGS YOU ARE'),
beats as (select * from all_beat where bar_id <= (select max(bar_id) from bars)),
fill as (
   select a.bar_id, a.beat_id, b.bar_id orig_bar_id, b.beat_id orig_beat_id,
          b.chord_id,
          lag(b.chord_id, 1) over (partition by a.bar_id order by a.beat_id) c1,
          lag(b.chord_id, 2) over (partition by a.bar_id order by a.beat_id) c2,
          lag(b.chord_id, 3) over (partition by a.bar_id order by a.beat_id) c3,
          lag(b.chord_id, 4) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c4,
          lag(b.chord_id, 5) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c5,
          lag(b.chord_id, 6) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c6,
          lag(b.chord_id, 7) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c7,
          lag(b.chord_id, 8) over (order by a.bar_id, a.beat_id range between unbounded preceding and current row) c8
   from beats                a
        left outer join bars b on (a.bar_id = b.bar_id and a.beat_id = b.beat_id)),
rc as (select bar_id, beat_id, coalesce(chord_id, c1, c2, c3, c4, c5, c6, c7, c8) chord_id
      from fill)
select bar_id, beat_id, chord_id
from rc
where chord_id is not null
order by bar_id, beat_id;

select * from bar where song_id = 1;

drop table chord;

create table chord (
   chord_id      varchar(20) not null primary key,
   root_midi_num integer,
   chord_form_cd varchar(10),
   root_note_cd  varchar(10),
   major_ind     char(1),
   midi1_num     integer,
   midi2_num     integer,
   midi3_num     integer,
   midi4_num     integer,
   midi5_num     integer,
   midi6_num     integer
);

drop table note;

create table note (
  note_cd varchar(10) not null primary key,
  midi_num integer
);

(save;

select bar_id, beat_id, chord_id, major_ind, midi1_num root, c.major_ind, midi1_num - 65
from bar              b
     left join chord c on (c.chord_id = b.chord_id)
where b.song_id = 1;

select * from note order by midi_num;

select * from chord;

select bar_id, beat_id, chord_id, major_ind, midi1_num root, c.major_ind,
       midi1_num -        lag(midi1_num) over (order by bar_id, beat_id),
       midi1_num - ( 12 + lag(midi1_num) over (order by bar_id, beat_id)),
       midi1_num - (-12 + lag(midi1_num) over (order by bar_id, beat_id))
from bar              b
     left join chord c on (c.chord_id = b.chord_id)
where b.song_id = 1;

-- New terminology:
-- chord ptrn       - base_line_chord
-- chord_ptrn_hdr   - base_line
-- bar_chord_ptrn_v - base_line_bar_v
-- chord_ptrn_v     - base_line_v

select * from bar where song_id = 1 order by bar_id, beat_id;

alter table chord add root_num integer;

select * from note;

drop table bass_line;

create table bass_line (
  bass_line_id varchar2(20) not null,
  bar_cnt      integer,
  bass_line_desc varchar(200),
  primary key (bass_line_id)
);

delete from bass_line;

insert into bass_line(bass_line_id, bar_cnt, bass_line_desc)
values
  ('Line-A',  4, 'Regular chord notes'),
  ('Line-B',  4, 'Better directs ear to next chord'),
  ('Line-C',  5, 'Downbeats of bars 2, 3, 4 start on third'),
  ('Line-D',  5, 'Common tones between chords where the low tones and high tones ascend together in parallel motion'),
  ('Line-E',  5, 'Classical approach - harmonic enclosure'),
  ('Line-F',  5, 'Classical approach - harmonic enclosure')
--  ('S251',    3, 'Sobolev p.14'),
--  ('S25',     2, 'Sobolev p.14'),
--  ('A25',     2, 'https://andylemaire.com/walking-bass/')
  ;

select * from bass_line;

create table bass_line_chord (
  bass_line_id varchar(20) not null,
  bar_id       integer     not null,
  beat_id      integer     not null,
  chord_id     varchar(20),
  primary key (bass_line_id, beat_id, bar_id)
);

select * from bass_line_chord;

delete from bass_line_chord;

insert into bass_line_chord (bass_line_id, bar_id, beat_id, chord_id)
values
  ('Line-A',  1, 1, 'DONOT-Cm7'), ('Line-A',  2, 1, 'Fm7'),  ('Line-A',  3, 1, 'Bb7'),    ('Line-A',  4, 1, 'Ebmaj7'),
  ('S251',    1, 1, 'Cm7'), ('S251',   2, 1, 'F7'),   ('S251',     3, 1, 'Bbmaj7'),
  ('Line-B',  1, 1, 'Fm7'), ('Line-B',  2, 1, 'Bbm7'), ('Line-B',  3, 1, 'Eb7'),    ('Line-B',  4, 1, 'Abmaj7'),
  ('Line-C',  1, 1, 'DOFm7'), ('Line-C',  2, 1, 'Bbm7'), ('Line-C',  3, 1, 'Eb7'),    ('Line-C',  4, 1, 'Abmaj7'), ('Line-C',  5, 1, 'Dbmaj7'),
  ('Line-D',  1, 1, 'DONOT-Fm7'), ('Line-D',  2, 1, 'Bbm7'), ('Line-D',  3, 1, 'Eb7'),    ('Line-D',  4, 1, 'Abmaj7'), ('Line-D',  5, 1, 'Dbmaj7'),
  ('Line-E',  1, 1, 'DOFm7'), ('Line-E',  2, 1, 'Bbm7'), ('Line-E',  3, 1, 'Eb7'),    ('Line-E',  4, 1, 'Abmaj7'), ('Line-E',  5, 1, 'Dbmaj7'),
  ('Line-F',  1, 1, 'Fm7'), ('Line-F',  2, 1, 'Bbm7'), ('Line-F',  3, 1, 'Eb7'),    ('Line-F',  4, 1, 'Abmaj7'), ('Line-F',  5, 1, 'Dbmaj7')
--  ('S25',     1, 1, 'DOCm7'),   ('S25',   2, 1, 'F7'),
--  ('A25',     1, 1, 'Am7'),   ('A25',   2, 1, 'D7')
  ;

drop table bass_line_note;

create table bass_line_note (
  bass_line_id  varchar(20),
  order_num     integer,
  note_cd       varchar(10),
  note_dur_num  float,
  primary key (bass_line_id, order_num)
);

-- Lines A, B, C, D, E, F from
-- https://www.learnjazzstandards.com/blog/learning-jazz/bass/keep-bass-lines-interesting/

delete from bass_line_note;

insert into bass_line_note (bass_line_id, order_num, note_cd, note_dur_num)
values
  ('Line-B', 1, 'F2',  4), ('Line-B', 2, 'G2', 4),  ('Line-B', 3, 'Ab2', 4),  ('Line-B', 4, 'A2', 4),
  ('Line-B', 5, 'Bb2', 4), ('Line-B', 6, 'C3', 4),  ('Line-B', 7, 'Db3', 4),  ('Line-B', 8, 'F3', 4),
  ('Line-B', 9, 'Eb3', 4), ('Line-B', 10, 'G3', 4), ('Line-B', 11, 'Bb3', 4), ('Line-B', 12, 'A3', 4),
  ('Line-B', 13, 'Ab3', 1),
  ('Line-C', 1,  'F2',  4),  ('Line-C', 2,  'G2',  4),  ('Line-C', 3, 'Ab2', 4),  ('Line-C', 4, 'C3', 4),
  ('Line-C', 5,  'Db2', 4),  ('Line-C', 6,  'F3',  4),  ('Line-C', 7, 'Bb3', 4),  ('Line-C', 8, 'Ab3', 4),
  ('Line-C', 9,  'G3',  4),  ('Line-C', 10, 'F3',  4),  ('Line-C', 11,'Eb3', 4),  ('Line-C', 12, 'Db3', 4),
  ('Line-C', 13, 'C3',  4),  ('Line-C', 14, 'Ab2', 4),  ('Line-C', 15,'Bb2', 4),  ('Line-C', 16, 'C3', 4),
  ('Line-C', 17, 'Db3', 1),
  ('Line-D', 1,  'C3',  4),  ('Line-D', 2,  'C3',  4),  ('Line-D', 3, 'F3', 4),   ('Line-D', 4, 'C3', 4),
  ('Line-D', 5,  'Db3', 4),  ('Line-D', 6,  'Db3', 4),  ('Line-D', 7, 'F3', 4),   ('Line-D', 8, 'Db3', 4),
  ('Line-D', 9,  'Eb3', 4),  ('Line-D', 10, 'Eb3', 4),  ('Line-D', 11,'G3', 4),   ('Line-D', 12, 'D3', 4),
  ('Line-D', 13, 'Eb3', 4),  ('Line-D', 14, 'Eb3', 4),  ('Line-D', 15,'Ab3', 4),  ('Line-D', 16, 'C3', 4),
  ('Line-D', 17, 'Db3', 1),
  ('Line-E', 1,  'F3',  4),  ('Line-E', 2,  'G3',  4),  ('Line-E', 3, 'F3', 4),   ('Line-E', 4, 'C3', 4),
  ('Line-E', 5,  'Db3', 4),  ('Line-E', 6,  'C3', 4),   ('Line-E', 7, 'Db3', 4),  ('Line-E', 8, 'Bb3', 4),
  ('Line-E', 9,  'Eb3', 4),  ('Line-E', 10, 'F3', 4),  ('Line-E', 11,'Eb3', 4),   ('Line-E', 12, 'Db3', 4),
  ('Line-E', 13, 'C3', 4),  ('Line-E', 14, 'B2', 4),  ('Line-E', 15,'C3', 4),  ('Line-E', 16, 'Ab2', 4),
  ('Line-E', 17, 'Db3', 1),
  ('Line-F', 1,  'F3',  4),  ('Line-F', 2,  'E3',  4),  ('Line-F', 3, 'F3', 4),   ('Line-F', 4, 'C3', 4),
  ('Line-F', 5,  'Db3', 4),  ('Line-F', 6,  'C3', 4),   ('Line-F', 7, 'Db3', 4),  ('Line-F', 8, 'Bb3', 4),
  ('Line-F', 9,  'Eb3', 4),  ('Line-F', 10, 'Db3', 4),  ('Line-F', 11,'C3', 4),   ('Line-F', 12, 'Bb3', 4),
  ('Line-F', 13, 'C3', 4),  ('Line-F', 14, 'B2', 4),  ('Line-F', 15,'C3', 4),  ('Line-F', 16, 'Ab2', 4),
  ('Line-F', 17, 'Db3', 1)
--  ('S251', 1,  'C3',  4),   ('S251', 2,  'C-2',  8/3),  ('S251', 3, 'Eb3', 8/3),
--  ('S251', 4,  'F3',  4),   ('S251', 5,  'C-2',  8/3),  ('S251', 6, 'A3', 8/3),
--  ('S251', 7,  'Bb3', 8/3), ('S251', 8, 'F3', 16),       ('S251', 9,'D3', 16),   ('S251', 10, 'Bb2', 4), ('S251', 11,  'C-2',  4)
--  ('S251', 1,  'C3',  4),  ('S251', 2,  'C#3',  4), ('S251', 3, 'D3', 4),  ('S251', 4,  'C#3',  4),
--  ('S251', 5,  'C3',  4),  ('S251', 6,  'A2',   4), ('S251', 7, 'F2', 4),  ('S251', 8,  'A2',  4),
--  ('S251', 9,  'Bb2', 4),  ('S251', 10, 'D2',   4), ('S251', 11, 'F2', 4), ('S251', 12,  'Eb3',  4),
--  ('S25', 1,  'C3',  4),   ('S25', 2,  'C-2',   4), ('S25', 3, 'G2', 4),   ('S25', 4,  'C-2',  4),
--  ('S25', 5,  'F2',  4),   ('S25', 6,  'C-2',   4), ('S25', 7, 'C3', 4),   ('S25', 8,  'C-2',  4),
--  ('A25', 1,  'A2',  4),   ('A25', 2,  'B2',    4), ('A25', 3, 'C3', 4),   ('A25', 4,  'C#3',  4),
--  ('A25', 5,  'D3',  4),   ('A25', 6,  'A2',    4), ('A25', 7, 'F#2', 4),  ('A25', 8,  'A2',  4)
  ;


select *
from bass_line_bar_v           a
     left join bass_line_bar_v b on (a.song_id = b.song_id and
                                     a.beg_bar_id < b.end_bar_id and
                                     a.end_bar_id > b.beg_bar_id and
                                     a.bass_line_id <> b.bass_line_id)
where
    a.song_id = b.song_id and
    a.song_id = 1 and
    b.song_id = 1;

select bass_line_id, beg_bar_id, end_bar_id
from bass_line_bar_v
where song_id = 1
order by beg_bar_id;

select * from bass_line_note where bass_line_id in ('Line-B', '251');


select n.midi_num, b.note_dur_num
from bass_line_note b
     join note      n on (n.note_cd = b.note_cd)
where b.bass_line_id = 'Line-B'
order by order_num;

select * from bass_line where bass_line_id = 'SMEDBLUES';
select * from bass_line_chord where bass_line_id = 'SMEDBLUES' order by 1, 2, 3;
select * from bass_line_note where bass_line_id = 'SMEDBLUES';

delete from bass_line       where bass_line_id = 'test';
delete from bass_line_chord where bass_line_id = 'test';
delete from bass_line_note  where bass_line_id = 'test';

delete from bass_line_chord where bass_line_id = '';

select * from bass_line_note;

select bass_line_id, beg_bar_id, end_bar_id, (transp_num - 12) transp_num
from bass_line_bar_v
where song_id = 7
order by beg_bar_id;

drop view bar_v;

create view bar_v
as
select b.*, c.root_midi_num, c.major_ind
from bar             b
     left join chord c on (c.chord_id = b.chord_id);

select * from bass_line_bar_v where song_id = 3 order by beg_bar_id;

select * from bass_line_v where bass_line_id = 'S251' and transp_num = 2;

select * from bass_line_note where bass_line_id = 'S251';

delete from bass_line       where bass_line_id like 'Milk%';
delete from bass_line_chord where bass_line_id like 'Milk%';
delete from bass_line_note  where bass_line_id like 'Milk%';

delete from bass_line       where bass_line_id like 'S25%';
delete from bass_line_chord where bass_line_id like 'S25%';
delete from bass_line_note  where bass_line_id like 'S25%';

delete from bass_line       where bass_line_id like 'A25%';
delete from bass_line_chord where bass_line_id like 'A25%';
delete from bass_line_note  where bass_line_id like 'A25%';

select * from bass_line_v where bass_line_id = 'S251' and transp_num = 0;

select * from note;

insert into note values ('_', 0);

select * from song;

alter table song add drum_ptrn_cd varchar(30);
alter table song add bass_ty_cd varchar(30);

update song set drum_ptrn_cd = 'drums-swing';
update song set bass_ty_cd = 'patterns';

create table song_drum (
  song_id            integer not null,
  bar_id             integer,
  drum_ptrn_cd       varchar(30)
  primary key(song_id, bar_id)
);
