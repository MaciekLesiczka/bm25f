type Field = {name:string; bag: string[]}

type FieldParam = {fieldName:string; w:float; b:float}

type Document = { Fields : Field[] }

type Corpus = { Documents : Document[] }

let getIDF corp = 
    let N = corp.Documents.Length |> float
    let idf Nd = N/Nd |> log
    let getWords doc = 
        doc.Fields |> Seq.collect (fun b -> b.bag) |> Set.ofSeq
    let idfDIct = corp.Documents |> Seq.collect getWords 
                                    |> Seq.countBy (fun x -> x) 
                                    |> Seq.map (fun (w,cnt) ->                                                                         
                                        (w, cnt |> float |> idf)) 
                                    |> Map.ofSeq
    fun x -> if idfDIct.ContainsKey(x) then idfDIct.[x] else 0.  


let getOccurrences corp =    
    let occurrences f = f.bag |> Seq.countBy (fun x -> x) |> Map.ofSeq
    let occurrDict = corp.Documents |> Seq.collect (fun x -> x.Fields) |> Seq.map (fun f -> (f.name, f|> occurrences)) |> Map.ofSeq
    (fun f t -> if occurrDict.ContainsKey(f.name) && occurrDict.[f.name].ContainsKey(t) then float occurrDict.[f.name].[t] else 0.)

let getTFD corpus fparams =
     let fAverage = corpus.Documents|> Seq.collect (fun x -> x.Fields) 
                                    |> Seq.groupBy (fun f -> f.name ) 
                                    |> Seq.map (fun (k,seq) -> (k, seq |> Seq.averageBy (fun b -> float b.bag.Length ))) 
                                    |> Map.ofSeq
     let ocurrences = getOccurrences corpus

     let TFD d t = 
        let fieldsDict = d.Fields |> Seq.map (fun f -> (f.name, f)) |> Map.ofSeq
        fparams |> Seq.map (fun f ->             
            let field = fieldsDict.[f.fieldName]
            let averagef = fAverage.[f.fieldName]
            let lengthf = float field.bag.Length
            let occurrencesf = (ocurrences field t)
            f.w*occurrencesf/(1.-f.b + f.b*lengthf/averagef)) |> Seq.sum
     TFD

let getBM25F corp fparams k1 = 
    let TFD = getTFD corp fparams
    let IDF = getIDF corp
    let bm25f d (q:string[]) =     
        let docTerms = d.Fields |> Seq.collect (fun f -> f.bag) //|> Seq.concat q |> Set.ofSeq        
        let terms = Seq.append docTerms q |> Set.ofSeq                
        terms |> Seq.map (fun t ->
            (IDF t)* (TFD d t) / ( k1 + (TFD d t))
        ) |> Seq.sum
    bm25f

//EXAMPLE

let title1 = { name="summary"; bag=[| "a"; "b"; "c"; "c";"a" |]}
let desc1 = { name="desc"; bag=[| "a"; "b"; "z" |]}
let doc1 = {Fields=[|title1;desc1|]}

let title2 = { name="summary"; bag=[| "a"; "b"; "c"; "c" |]}
let desc2 = { name="desc"; bag=[| "x"; "y"; "z" |]}
let doc2 = {Fields=[|title2;desc2|]}

let title3 = { name="summary"; bag=[| "w"; "f"; "c" |]}
let desc3 = { name="desc"; bag=[| "x"; "y"; "z";"x" |]}
let doc3 = {Fields=[|title3;desc3|]}

let corpus = {Documents = [|doc1;doc2;doc3|]}    

let fieldParams = [|{fieldName ="summary";w=1.;b=1.};{fieldName="desc";w=1.;b=1.}|] |> Seq.ofArray

let k1 = 0.8
let BM25F = getBM25F corpus fieldParams k1
BM25F doc2 [|"a"; "b"|]