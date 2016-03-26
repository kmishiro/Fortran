subroutine output(text, linebreak) ! �^�[�~�^���ƃt�@�C��(file.01)�ւ̏o��

  use setParameters, only : &
       ON, OFF, DIS, &
       BLACK, WHITE, &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       DiscsWHITE, DiscsBLACK, &
       CurrentColor, &
       RawBoard, &
       Turns

  implicit none

  character(len=*) &
       text ! �e�L�X�g�i�����j
  integer &
       lineBreak ! ���s����(ON)�����Ȃ���(OFF)�A�������͔Ֆʂ��o��(DIS)

  integer &
       x, y, &
       color

  if(lineBreak == OFF) then ! ���s���Ȃ��ꍇ

     write(01, '(a)', advance='no') Text ! �e�L�X�g�̏o�́i�t�@�C���j
     write(06, '(a)', advance='no') Text ! �e�L�X�g�̏o�́i�^�[�~�i���j

  else if(lineBreak == ON) then ! ���s����ꍇ

     write(01, '(a)') Text ! �e�L�X�g�̏o�́i�t�@�C���j
     write(06, '(a)') Text ! �e�L�X�g�̏o�́i�^�[�~�i���j

  else if(lineBreak == DIS) then ! �Ֆʂ̏o��

     write(01, '(a)', advance='no') 'Turn='
     write(06, '(a)', advance='no') 'Turn='
     select case (CurrentColor)
     case(WHITE)
        write(01, '(a)', advance='no') 'O, '
        write(06, '(a)', advance='no') 'O, '
     case(BLACK)
        write(01, '(a)', advance='no') 'X, '
        write(06, '(a)', advance='no') 'X, '
     end select
     write(01, '(a,i2)') 'TurnNumber=', Turns
     write(06, '(a,i2)') 'TurnNumber=', Turns

     write(01, '(a)') ' ------------------ '
     write(06, '(a)') ' ------------------ '
     write(01, '(a)') '|  a b c d e f g h |'
     write(06, '(a)') '|  a b c d e f g h |'
     do y=BOARD_BGN, BOARD_END

        write(01, '(a,i1)', advance='no') '|', y-1
        write(06, '(a,i1)', advance='no') '|', y-1

        do x=BOARD_BGN, BOARD_END

           color=RawBoard(x, y)

           select case (color)
           case(WHITE)
              write(01, '(a)', advance='no') ' O'
              write(06, '(a)', advance='no') ' O'
           case(BLACK)
              write(01, '(a)', advance='no') ' X'
              write(06, '(a)', advance='no') ' X'
           case default
              write(01, '(a)', advance='no') '  '
              write(06, '(a)', advance='no') '  '
           end select

        end do

        write(01, '(a)') ' |'
        write(06, '(a)') ' |'
     end do
     write(01, '(a)') ' ------------------ '
     write(06, '(a)') ' ------------------ '

     write(01, '(a,i2)', advance='no') 'BLACK=', DiscsBLACK
     write(06, '(a,i2)', advance='no') 'BLACK=', DiscsBLACK
     write(01, '(a,i2)') ', WHITE=', DiscsWHITE
     write(06, '(a,i2)') ', WHITE=', DiscsWHITE

  else

     write(01, '(a)') 'program bug:: s:output' ! breakline��ON, OFF, DIS�ȊO�������Ă���ƃG���[
     write(06, '(a)') 'program bug:: s:output'
     stop

  end if

  return
end subroutine output

!----------------------------------------------------------------

subroutine writePutPlace ! �����ۑ��p�̏o��(file.02)
                         ! �Q�[���I����A���������𐔒l���當���ɕϊ����ăt�@�C���ɏo��
  use setParameters, only : &
       BOARD_BGN, BOARD_END
  use setVariables, only : &
       MemoryPutPlace, &
       Turns

  implicit none

  integer &
       x, y, &
       turnsv
  character(len=1) &
       inx, iny

  do turnsv=1, Turns-1

     x=int(MemoryPutPlace(turnsv) /10)-1
     y=mod(MemoryPutPlace(turnsv), 10)-1

     select case (x)
     case(1); inx='a'
     case(2); inx='b'
     case(3); inx='c'
     case(4); inx='d'
     case(5); inx='e'
     case(6); inx='f'
     case(7); inx='g'
     case(8); inx='h'
     case default
        write(01, '(a)') 'program bug:: s:writePutPlace(X)'
        write(06, '(a)') 'program bug:: s:writePutPlace(X)'
        stop
     end select

     write(iny, '(i1)') y
     if(y.lt.BOARD_BGN.and.y.gt.BOARD_END) then
        write(01, '(a)') 'program bug:: s:writePutPlace(Y)'
        write(06, '(a)') 'program bug:: s:writePutPlace(Y)'
        stop
     end if

     write(02, '(a)', advance='no') inx//iny

  end do

  return
end subroutine writePutPlace
