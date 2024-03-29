%% Author: Dru
%% Created: 6 Dec 2010
%% Description: TODO: Add description to main
-module(case_study).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([distributed_benchmark/1, benchmark/1, benchmark_code/1, monitor_property_1/0, violate_property_1/0, monitor_property_2/0, violate_property_2/0, monitor_property_3/0, violate_property_3/0]).

%%
%% API Functions
%%

distributed_benchmark(Num) ->
    erlang:set_cookie('abc@ubuntu.ubuntu-domain', erlang),
    timer:sleep(1000),
    net_kernel:connect('def@Rudolph-PC.internet.vodafone.com.mt'),
    timer:sleep(1000),
    case_study:benchmark(Num).

benchmark(Num) ->
    %backend_supervisor:start_link(global:whereis_name(tracer)),
    backend:start_link(),
    timer:sleep(1000),
    %backend_users_fsm:start_link(),
    %timer:sleep(1000),
    backend_trace:start_link(),
    timer:sleep(1000),
    timer:tc(case_study, benchmark_code, [Num]).

benchmark_code(0) ->
    ok;
benchmark_code(Num) ->
    Doctor = erlang:list_to_atom("Doctor" ++ erlang:integer_to_list(Num)),
    Patient = erlang:list_to_atom("Patient" ++ erlang:integer_to_list(Num)),
    backend ! {createDoctor, Doctor},
    backend ! {createPatient, Patient},
    benchmark_code(Num - 1).

monitor_property_1() ->
    backend ! {createDoctor, 'Dr. Abela'},
    timer:sleep(1000),
    backend ! {createPatient, 'Andrew'},
    timer:sleep(1000),
    'Dr. Abela' ! {addPatient, 'Andrew'},
    timer:sleep(1000),
    'Andrew' ! {requestRecord},
    timer:sleep(1000),
    'Dr. Abela' ! {sendResponse, 'Andrew', approve}.

violate_property_1() ->
    backend ! {createDoctor, 'Dr. Abela'},
    timer:sleep(1000),
    backend ! {createPatient, 'Andrew'},
    timer:sleep(1000),
    'Dr. Abela' ! {addPatient, 'Andrew'},
    timer:sleep(1000),
    io:format("*--- Response sent manually for patient - 'Andrew' - where response is approve.~n", []),
    'Andrew' ! {backend_response,{'Andrew',fever}}.

monitor_property_2() ->
    backend ! {createDoctor, 'Dr. Abela'},
    timer:sleep(1000),
    backend ! {createDoctor, 'Dr. Sammut'},
    timer:sleep(1000),
    backend ! {createPatient, 'Andrew'},
    timer:sleep(1000),
    'Dr. Abela' ! {addPatient, 'Andrew'},
    timer:sleep(1000),
    'Dr. Sammut' ! {addPatient, 'Andrew'},
    timer:sleep(1000),
    'Andrew' ! {requestRecord},
    timer:sleep(1000),
    'Dr. Abela' ! {sendResponse, 'Andrew', approve},
    timer:sleep(1000),
    'Dr. Sammut' ! {sendResponse, 'Andrew', approve}.

violate_property_2() ->
    backend ! {createDoctor, 'Dr. Abela'},
    timer:sleep(1000),
    backend ! {createPatient, 'Andrew'},
    timer:sleep(1000),
    'Dr. Abela' ! {addPatient, 'Andrew'},
    timer:sleep(1000),
    'Andrew' ! {requestRecord},
    timer:sleep(1000),
    io:format("*--- Response sent manually for patient - 'Andrew' - where response is approve.~n", []),
    timer:sleep(1000),
    'Andrew' ! {backend_response,{'Paul',fever}}.

monitor_property_3() ->
    backend ! {createDoctor, 'Dr. Abela'},
    timer:sleep(1000),
    backend ! {createDoctor, 'Dr. Sammut'},
    timer:sleep(1000),
    backend ! {createPatient, 'Andrew'},
    timer:sleep(1000),
    'Dr. Abela' ! {addPatient, 'Andrew'},
    timer:sleep(1000),
    'Dr. Sammut' ! {addPatient, 'Andrew'},
    timer:sleep(1000),
    'Andrew' ! {requestRecord},
    timer:sleep(1000),
    'Dr. Abela' ! {sendResponse, 'Andrew', approve},
    timer:sleep(1000),
    'Dr. Sammut' ! {sendResponse, 'Andrew', approve}.

violate_property_3() ->
    backend ! {createDoctor, 'Dr. Abela'},
    timer:sleep(1000),
    backend ! {createDoctor, 'Dr. Sammut'},
    timer:sleep(1000),
    backend ! {createPatient, 'Andrew'},
    timer:sleep(1000),
    'Dr. Abela' ! {addPatient, 'Andrew'},
    timer:sleep(1000),
    'Dr. Sammut' ! {addPatient, 'Andrew'},
    timer:sleep(1000),
    'Andrew' ! {requestRecord},
    timer:sleep(1000),
    'Dr. Abela' ! {sendResponse, 'Andrew', approve},
    timer:sleep(1000),
    'Andrew' ! {backend_response,{'Andrew',fever}}.


