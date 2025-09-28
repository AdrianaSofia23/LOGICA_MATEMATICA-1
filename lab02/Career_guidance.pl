% Career guidance expert system


% ---------------------------
% Step 1: Knowledge base
% ---------------------------

%Define careers

career(agroindustry).
career(biology).
career(biomedicine).
career(computer_Science).
career(mathematics).
career(data_Science).
career(chemistry).
career(materiasl_Science).
career(biochemistry_and_Pharmacy).
career(nanotechnology).
career(physics).
career(geology).

% Related areas of interest
related_areas(agroindustry,[technology_food, agribusiness, industrial_processes]).
related_areas(biology, [living_organisms, nature, evolution]).
related_areas(biomedicine, [human_health, medicine, biology]).
related_areas(computer_Science, [programming, algorithms, problem_solving, mathematics]).
related_areas(mathematics, [logic, problem_solving, theory]).
related_areas(data_Science, [data_analysis, statistics, programming]).
related_areas(chemistry, [matter, reactions, substances]).
related_areas(materials_Science, [materials, properties, physics_chemistry]).
related_areas(biochemistry_and_Pharmacy, [medicines, biochemistry, health]).
related_areas(nanotechnology, [nanoscale, innovation, materials, physics]).
related_areas(physics, [universe, laws_nature, mathematics]).
related_areas(geology, [earth, field_work, environment, resources]).

% ---------------------------
% Step 2: Rules of classification
% ---------------------------

% 1. Basic Sciences
% Degrees focused on understanding nature, life, and physical phenomena:
basic_sciences(Career) :-
    related_areas(Career, Interests),
    (member(nature, Interests) ;        % biology
     member(reactions, Interests) ;     % chemistry
     member(universe, Interests) ;   % physics
     member(earth, Interests) ;         % geology
     member(logic, Interests)           % mathematics
    ).

% 2. Applied Sciences
% Degrees that apply scientific knowledge to health, food, or materials:
applied_sciences(Career) :-
    related_areas(Career, Interests),
    (member(technology_food, Interests) ;   % agroindustry
     member(medicine, Interests) ;          % biomedicine
     member(biochemistry, Interests) ;      % biochemistry_and_pharmacy
     member(properties, Interests) ;        % materials_science
     member(nanoscale, Interests)           % nanotechnology
    ).

% 3. Technology/Computing
% Degrees focused on computing, data analysis, and technological development:
technology(Career) :-
    related_areas(Career, Interests),
    (member(algorithms, Interests) ;    % computer_science
     member(statistics, Interests)      %data_science
    ).


% ---------------------------
% Step 3: Interactive Questions 
% ---------------------------

% ---------------------------
% SIMPLE QUESTIONNAIRE 
% ---------------------------

start_questionnaire :-
    write('=== UNIVERSITY CAREER ADVISOR ==='), nl,
    write('Answer yes or no to the following questions:'), nl, nl,
    
    ask('Do you enjoy basic sciences (nature, chemistry, physics, geology, mathematics)?', Answer1), nl,
    ask('Are you interested in applied sciences (health, food, materials, nanotechnology)?', Answer2), nl,
    ask('Do you like technology and computing?', Answer3),
    
    % Find careers
    findall(Career, (
        career(Career),
        ( (Answer1 == yes, basic_sciences(Career)) ;
          (Answer2 == yes, applied_sciences(Career)) ;
          (Answer3 == yes, technology(Career)) )
    ), AllCareers),
    
    % Remove duplicates
    remove_duplicates(AllCareers, Careers),
    
    % Show results
    ( Careers = [] -> 
        write('No careers match your selected interests.'), nl
    ; 
        nl, write('=== RECOMMENDED CAREERS ==='), nl,
        display_careers_list(Careers)
    ).

remove_duplicates(List, Unique) :-
    remove_duplicates(List, [], Unique).

remove_duplicates([], Acc, Acc).
remove_duplicates([H|T], Acc, Unique) :-
    ( member(H, Acc) ->
        remove_duplicates(T, Acc, Unique)
    ;
        remove_duplicates(T, [H|Acc], Unique)
    ).

% ask/2
ask(Question, Answer) :-
    write(Question), 
    write(' (yes/no): '),
    read(Answer).

% display_careers_list
display_careers_list([]) :- true.  
display_careers_list([Career|Rest]) :-
    write('✓ '), write(Career), nl,
    display_careers_list(Rest).


% ---------------------------
% ADVANCED QUESTIONNAIRE 
% ---------------------------

% main predicate 
advanced_questionnaire :-
    write('=== ADVANCED CAREER ADVISOR ==='), nl,
    write('Select your main interest area:'), nl,
    write('1. Basic Sciences'), nl,
    write('2. Applied Sciences'), nl,
    write('3. Technology'), nl,
    write('Your choice (1/2/3): '),
    read(Choice),
    
    ( Choice = 1 -> filter_basic_sciences ;
      Choice = 2 -> filter_applied_sciences ;
      Choice = 3 -> filter_technology ;
      write('Invalid choice'), nl, fail
    ).


filter_basic_sciences :-
    write('=== BASIC SCIENCES ==='), nl,
    
    findall(Career, basic_sciences(Career), BasicCareers),
    write('Available careers: '), write(BasicCareers), nl, nl,
    
    get_basic_sciences_interests(BasicInterests),
    write('Select your interests from the list below:'), nl,
    display_interests_menu(BasicInterests),
    
  
   write('Enter the numbers of your three interests as a list (e.g., [1,3,5]): '),
    read(SelectedNumbers),
    
    (is_list(SelectedNumbers) -> 
        convert_numbers_to_interests(SelectedNumbers, BasicInterests, UserInterests)
    ; 
        write('Invalid input. Please enter a list like [1,3,5]'), nl,
        UserInterests = []
    ),
    
    nl, write('Your selected interests: '), write(UserInterests), nl,
    
    (UserInterests = [] -> 
        write('No interests selected.') 
    ; 
        find_matching_careers(BasicCareers, UserInterests, MatchingCareers),
        display_career_matches(MatchingCareers)
    ).

filter_applied_sciences :-
    write('=== APPLIED SCIENCES ==='), nl,
    
    findall(Career, applied_sciences(Career), AppliedCareers),
    write('Available careers: '), write(AppliedCareers), nl, nl,
    
    get_applied_sciences_interests(AppliedInterests),
    write('Select your interests from the list below:'), nl,
    display_interests_menu(AppliedInterests),
    
   write('Enter the numbers of your three interests as a list (e.g., [1,3,5]): '),
    read(SelectedNumbers),
    
    (is_list(SelectedNumbers) -> 
        convert_numbers_to_interests(SelectedNumbers, AppliedInterests, UserInterests)
    ; 
        write('Invalid input. Please enter a list like [1,3,5]'), nl,
        UserInterests = []
    ),
    
    nl, write('Your selected interests: '), write(UserInterests), nl,
    
    (UserInterests = [] -> 
        write('No interests selected.') 
    ; 
        find_matching_careers(AppliedCareers, UserInterests, MatchingCareers),
        display_career_matches(MatchingCareers)
    ).

filter_technology :-
    write('=== TECHNOLOGY/COMPUTING ==='), nl,
    
    findall(Career, technology(Career), TechCareers),
    write('Available careers: '), write(TechCareers), nl, nl,
    
    get_technology_interests(TechInterests),
    write('Select your interests from the list below:'), nl,
    display_interests_menu(TechInterests),
    

    write('Enter the numbers of your three interests as a list (e.g., [1,3,5]): '),
    read(SelectedNumbers),
    

    (is_list(SelectedNumbers) -> 
        convert_numbers_to_interests(SelectedNumbers, TechInterests, UserInterests)
    ; 
        write('Invalid input. Please enter a list like [1,3,5]'), nl,
        UserInterests = []
    ),
    
    nl, write('Your selected interests: '), write(UserInterests), nl,
    
    (UserInterests = [] -> 
        write('No interests selected.') 
    ; 
        find_matching_careers(TechCareers, UserInterests, MatchingCareers),
        display_career_matches(MatchingCareers)
    ).


get_basic_sciences_interests(Interests) :-
    findall(Interest, (
        basic_sciences(Career),
        related_areas(Career, CareerInterests),
        member(Interest, CareerInterests)
    ), InterestsList),
    sort(InterestsList, Interests).

get_applied_sciences_interests(Interests) :-
    findall(Interest, (
        applied_sciences(Career),
        related_areas(Career, CareerInterests),
        member(Interest, CareerInterests)
    ), InterestsList),
    sort(InterestsList, Interests).

get_technology_interests(Interests) :-
    findall(Interest, (
        technology(Career),
        related_areas(Career, CareerInterests),
        member(Interest, CareerInterests)
    ), InterestsList),
    sort(InterestsList, Interests).

display_interests_menu(Interests) :-
    display_interests_with_numbers(Interests, 1).

display_interests_with_numbers([], _).
display_interests_with_numbers([Interest|Rest], N) :-
    write(N), write('. '), write(Interest), nl,
    Next is N + 1,
    display_interests_with_numbers(Rest, Next).

convert_numbers_to_interests([], _, []).
convert_numbers_to_interests([N|Rest], AllInterests, [Interest|Interests]) :-
    nth1(N, AllInterests, Interest),
    convert_numbers_to_interests(Rest, AllInterests, Interests).

find_matching_careers(Careers, UserInterests, MatchingCareers) :-
    findall(Career-Score, (
        member(Career, Careers),
        related_areas(Career, CareerInterests),
        count_matching_interests(UserInterests, CareerInterests, Score),
        Score > 0
    ), CareersWithScores),
    
    predsort(compare_scores, CareersWithScores, SortedCareers),
    findall(Career, member(Career-_, SortedCareers), MatchingCareers).

count_matching_interests(UserInterests, CareerInterests, Count) :-
    intersection(UserInterests, CareerInterests, Common),
    length(Common, Count).

compare_scores(>, _-Score1, _-Score2) :- Score1 > Score2.
compare_scores(<, _-Score1, _-Score2) :- Score1 < Score2.

display_career_matches([]) :-
    nl, write('No careers match your selected interests.'), nl.

display_career_matches(MatchingCareers) :-
    nl, write('=== RECOMMENDED CAREERS ==='), nl,
    display_careers_with_interests(MatchingCareers).

display_careers_with_interests([]).



display_careers_with_interests([Career|Rest]) :-
    related_areas(Career, Interests),
    write(' --> '), write(Career), nl,
    write('   Interests: '), write(Interests), nl, nl,
    display_careers_with_interests(Rest).
