

let salaries = [75000; 48000; 120000; 190000; 300113; 92000; 36000]


printfn "Salaries: %A" salaries


let highIncomeSalaries = salaries |> List.filter (fun x -> x > 100000)
printfn "\nHigh-income salaries: %A" highIncomeSalaries


let calculateTax salary =
    if salary <= 49020 then 
        float salary * 0.15
    elif salary <= 98040 then 
        float salary * 0.205
    elif salary <= 151978 then 
        float salary * 0.26
    elif salary <= 216511 then 
        float salary * 0.29
    else 
        float salary * 0.33


let taxes = salaries |> List.map calculateTax
printfn "\nTaxes for each salary: %A" taxes

let updatedSalaries = 
    salaries 
    |> List.filter (fun salary -> salary < 49020) 
    |> List.map (fun salary -> salary + 20000)


printfn "\nUpdated Salaries (less than $49,020 increased by $20,000): %A" updatedSalaries


let salariesInRange = salaries |> List.filter (fun x -> x >= 50000 && x <= 100000)
let totalSalariesInRange = salariesInRange |> List.sum
printfn "\nSum of salaries between $50,000 and $100,000: %d" totalSalariesInRange






let sumOfMultiplesOf3 n =
    let rec sumHelper current sum =
        if current > n then
            sum
        else
            sumHelper (current + 3) (sum + current)
    sumHelper 3 0


let result = sumOfMultiplesOf3 30
printfn "The sum of all multiples of 3 up to 30 is: %d" result