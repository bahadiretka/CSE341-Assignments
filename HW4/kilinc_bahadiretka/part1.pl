:- dynamic(instructor/3).
:- dynamic(occupancy/2).
:- dynamic(registered_student/2).
:- dynamic(student/3).
:- dynamic(room/3).
:- dynamic(course/6).


student(1, [cse102, cse331], yes).
student(2, [cse102], no).
student(3, [cse102, cse321], no).
student(4, [cse102, cse321], no).
student(5, [cse343, cse321], no).
student(6, [cse343, cse321], no).
student(7, [cse343, cse321], no).
student(8, [cse343, cse331], yes).
student(9, [cse102, cse343, cse331], no).
student(10, [cse102, cse343], no).
student(11, [cse102, cse331], no).
student(12, [cse102], no).
student(13, [cse343, cse331], no).
student(14, [cse102, cse343, cse331], yes).
student(15, [cse102, cse343], no).

occupancy(z23, 8, cse102).
occupancy(z23, 9, cse102).
occupancy(z23, 10, cse102).
occupancy(z23, 11, cse102).

occupancy(z23, 13, cse331).
occupancy(z23, 14, cse331).
occupancy(z23, 15, cse331).

occupancy(z11, 8, cse343).
occupancy(z11, 9, cse343).
occupancy(z11, 10, cse343).
occupancy(z11, 11, cse343).

occupancy(z11, 14, cse321).
occupancy(z11, 15, cse321).
occupancy(z11, 16, cse321).

occupancy(z23, 8, cse331).

instructor(genc, cse102, projector).
instructor(turker, cse343, smartboard).
instructor(bayrakci, cse331, _).
instructor(gozupek, cse321, smartboard).

course(cse102, genc, 10, 4, z23, projector).
course(cse343, turker, 6, 3, z11, no_needs).
course(cse331, bayrakci, 5, 3, z23, no_needs).
course(cse321, gozupek, 10, 4, z11, no_needs).

room(z23, 10, [hcapped, projector]).
room(z11, 10, [hcapped, smartboard]).


conflicts(X, Y) :-
  occupancy(A, Z, X),
  occupancy(A, Z, Y),
  not(X = Y).


assign(RoomId, CourseId) :-
  room(RoomId, Capacity, A), 
  course(CourseId, _, Capacity, _, RoomId, B),
  instructor(_, CourseId, Need),
  (member(B, A); member(A, B); (A = B); (B = no_needs)),
  (member(Need, A); (Need = B); (Need = A)).

enroll(StudentId, CourseId) :-
  student(StudentId, CourseList, H),
  room(RoomId, _, NeedList),
  course(CourseId, _, _, _, RoomId, _),
  member(CourseId, CourseList),
  (
     (   (H = yes), member(hcapped, NeedList) );
         (H = no)
  ).