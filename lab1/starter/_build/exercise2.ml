open Core

exception Unimplemented

type weather_type = Sunny | Cloudy | Rainy of float
type forecast = {
  weather: weather_type;
  temperature: float;
  humidity: float;
}

let main () =
  let weather_to_string (w : weather_type) : string =
    match w with
    | Sunny -> "Sunny"
    | Cloudy -> "Cloudy"
    | Rainy n -> Printf.sprintf "Rainy(%.1f)" n
  in

  let forecast_to_string (f : forecast) : string =
    Printf.sprintf "{weather: %s, temp: %.1f, humidity: %.1f}"
      (weather_to_string f.weather) f.temperature f.humidity
  in

  let example_forecasts = [
    {weather = Sunny; temperature = 70.; humidity = 0.0};
    {weather = Cloudy; temperature = 62.; humidity = 0.3};
    {weather = Rainy 0.7; temperature = 55.; humidity = 0.5};
    {weather = Cloudy; temperature = 68.; humidity = 0.1};
    {weather = Rainy 0.4; temperature = 52.; humidity = 0.8};
    {weather = Rainy 0.7; temperature = 59.; humidity = 0.2};
  ] in

  let just_temp (l : forecast list) : float list =
    List.map ~f:(fun (x : forecast) : float -> x.temperature) l
  in

  assert (just_temp example_forecasts = [70.; 62.; 55.; 68.; 52.; 59.]);

  let average_temp (l : forecast list) : float =
    List.fold_left l ~init:0. ~f:(fun (accum : float) (n: forecast) : float ->
                                    accum +. n.temperature /. (float (List.length l)))
  in

  assert (average_temp example_forecasts = 61.);

  (* Rainy above 70% and humidity over 50% *)
  let humid_rainy_days (l : forecast list) : forecast list =
    let check_rain (weather: weather_type) : bool  =
      match weather with
      | Rainy v -> v >= 0.7
      | _ -> false
    in
      List.filter ~f:(fun (y : forecast) : bool -> (check_rain y.weather) && y.humidity >= 0.5) l
  in

  assert (humid_rainy_days example_forecasts = [
    {weather = Rainy 0.7; temperature = 55.; humidity = 0.5}]);

  let weather_histogram (l : forecast list) : (weather_type * int) list =
    List.fold_left l ~init:[] ~f:(fun (accum : (weather_type * int) list) (x : forecast) : (weather_type * int) list ->
      if List.length (List.filter accum ~f:(fun ((t, n) : weather_type * int) : bool ->
        t = x.weather)) > 0 then
        List.map accum ~f:(fun ((t, n) : (weather_type * int)) : (weather_type * int) ->
          if t = x.weather then
            (t, n + 1)
          else
            (t, n)
        )
      else
        (x.weather, 1) :: accum
    )
  in

  assert (weather_histogram example_forecasts = [
    (Rainy 0.4, 1);
    (Rainy 0.7, 2);
    (Cloudy, 2);
    (Sunny, 1);
  ])

let () = main ()
