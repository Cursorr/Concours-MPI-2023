let caillou_vide chemin =
  let rec aux index =
    if chemin.(index) = 0 then
      index
    else
      aux (index + 1)
  in
  aux 0

let echange chemin index1 index2 =
  let chemin_copie = Array.copy chemin in
  let temp = chemin_copie.(index1) in
  chemin_copie.(index1) <- chemin_copie.(index2);
  chemin_copie.(index2) <- temp;
  chemin_copie

let randonneurG_avance chemin =
  let index = caillou_vide chemin in
  index > 0 && chemin.(index - 1) = 1

let randonneurG_saute chemin =
  let index = caillou_vide chemin in
  index > 1 && chemin.(index - 2) = 1

let mouvement_chemin chemin =
  let rec mouvement_gauche index =
    if index >= Array.length chemin then
      []
    else if chemin.(index) = 1 then
      match caillou_vide chemin with
      | n when n = index -> mouvement_gauche (index + 1)
      | n when n = index + 1 -> (echange chemin index n) :: mouvement_gauche (index + 1)
      | n when n = index + 2 -> (echange chemin index n) :: mouvement_gauche (index + 2)
      | _ -> mouvement_gauche (index + 1)
    else
      mouvement_gauche (index + 1)
  in
  let rec mouvement_droite index =
    if index >= Array.length chemin then
      []
    else if chemin.(index) = 2 then
      match caillou_vide chemin with
      | n when n = index -> mouvement_droite (index + 1)
      | n when n = index - 1 -> (echange chemin index n) :: mouvement_droite (index + 1)
      | n when n = index - 2 -> (echange chemin index n) :: mouvement_droite (index + 2)
      | _ -> mouvement_droite (index + 1)
    else
      mouvement_droite (index + 1)
  in
  mouvement_gauche 0 @ mouvement_droite 0

    
let passage nG nD =
  let chemin_initial = Array.append (Array.make nG 1) (Array.append [|0|] (Array.make nD 2)) in
  let rec aux etats =
    match etats with
    | [] -> failwith "Pas de solution"
    | etat :: autres_etats ->
        if caillou_vide etat = Array.length etat - 1 then
          caillou_vide etat
        else
          aux (mouvement_chemin etat @ autres_etats)
  in
  aux [chemin_initial]

