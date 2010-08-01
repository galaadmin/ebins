%% Author: eschneef
%% Created: Sep 19, 2008
%% Description: TODO: Add description to bins
-module(bins).

-export([start/0]).

start() ->
    io:fwrite("this is it", []),
    Fprint = fun(Bins, Boxes) ->
                     lists:foreach(
                       fun({bin, BinId, Rem, BoxList}) -> 
                               [Box|List] = lists:reverse(BoxList),
                               io:fwrite("id: ~w, rem: ~w, boxes: ~w", [BinId, Rem, Box]),
                               lists:foreach(fun(X) -> io:fwrite(", ~w", [X]) end, List),
                               io:nl()
                       end, Bins),
                     case Boxes of
                         [] ->
                             ok;
                         Boxes ->
                             io:fwrite("remaining ~w boxes ~w~n", [length(Boxes), Boxes])
                     end
             end,
    
    case file:open("bins.txt", [read]) of
        {ok, Fid} ->
            {Bins, Boxes} = parse(Fid),
            file:close(Fid),
            case pack(length(Boxes), Bins, [], Boxes) of
                {failed, B, R} ->
                    io:fwrite("failed~n", []),
                    Fprint(B, R);
                {done, B} ->
                    io:fwrite("done~n", []),
                    Fprint(B, []);
                Other ->
                    io:fwrite("Other ~w~n", [Other])
            end;
        {error, Error} ->
            {error, Error}
    end.

parse(Fid) ->
    {ok, [Bins]} = io:fread(Fid, '', "~d"),
    {ok, [Size]} = io:fread(Fid, '', "~d"),
    {lists:map(fun(N) -> {bin, N, trunc(Size), []} end, lists:seq(1, Bins)), 
     lists:reverse(lists:sort(get_boxes(Fid, [])))}.

get_boxes(Fid, Boxes) ->
    case io:fread(Fid, '', "~d") of
        eof ->
            %% io:fwrite("got eof~n", []),
            Boxes;
        {error, {fread, integer}} ->
            Boxes;
        {ok, [Box]} ->
            get_boxes(Fid, [Box|Boxes]);
        Other ->
            io:fwrite("~w~n", [Other])
    end.

pack(0, OBins, NBins, Boxes) ->
    {failed, lists:keysort(2, OBins ++ NBins), Boxes};
pack(_, OBins, NBins, []) ->
    {done, lists:keysort(2, OBins ++ NBins)};
pack(N, [], Bins, Boxes) ->
    pack(N, lists:reverse(lists:keysort(3, Bins)), [], Boxes);
pack(N, OBins = [{bin, BinId, Remaining, In}|Bins], NewBins, OBoxes = [Box|Boxes]) ->
    case lists:member(Remaining, OBoxes) of
        true ->
            pack(N, Bins, [{bin, BinId, 0, [Remaining|In]}|NewBins], OBoxes -- [Remaining]);
        false ->
            case Remaining - Box of
                Space when Space < 0 ->
                    pack(N-1, OBins, NewBins, rotate(OBoxes));
                Space ->
                    pack(N, Bins, [{bin, BinId, Space, [Box|In]}|NewBins], Boxes)
            end
    end.

rotate([H|T]) -> T ++ [H].
