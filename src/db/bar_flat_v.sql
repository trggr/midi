drop view bar_flat;

create view bar_flat
as
select b.song_id, b.bar_id, b.beat_id, b.chord_id,
       s.song_nm, s.time_sig_nmrtr_num, s.time_sig_denom_num,
       s.time_sig_ppq_num, s.time_sig_bb_num  
from bar             b
     inner join song s on (s.song_id = b.song_id);
                 
