drop view bass_line_bar_v;

create view bass_line_bar_v as
with recursive
s (song_id, bass_line_id, beg_bar_id, next_bar_id, chord_id, step_num, bar_cnt,
   ptrn_root_midi_num, transp_root_midi_num, transp_num) as (
         select b.song_id,
                p.bass_line_id,
                b.bar_id        beg_bar_id,
                b.bar_id + 1    next_bar_id,
                b.chord_id,
                p.bar_id        step_num,
                p.bar_cnt,
                p.ptrn_root_midi_num,
                p.transp_root_midi_num,
                p.transp_num
         from bar              b
              join bass_line_v p on (p.chord_id = b.chord_id and
                                     p.beat_id  = b.beat_id)
         where p.bar_id  = 1
         union all
         select s.song_id,
                s.bass_line_id,
                s.beg_bar_id,
                b.bar_id + 1,
                b.chord_id,
                p.bar_id,
                s.bar_cnt,
                s.ptrn_root_midi_num,
                s.transp_root_midi_num,
                s.transp_num
         from s
              join bar         b  on (b.song_id  = s.song_id  and
                                      b.bar_id   = s.next_bar_id)
              join bass_line_v p  on (p.chord_id     = b.chord_id   and
                                      p.beat_id      = b.beat_id    and
                                      p.bass_line_id = s.bass_line_id)
       where p.bar_id = s.step_num + 1)
select song_id, bass_line_id, beg_bar_id, next_bar_id - 1 end_bar_id, bar_cnt,
       ptrn_root_midi_num, transp_root_midi_num, transp_num
from s
where step_num = bar_cnt
order by song_id, bar_cnt desc, beg_bar_id;
