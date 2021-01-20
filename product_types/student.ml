type student = { first_name : string ; last_name : string ; gpa : float }

let kev = { first_name="Kevin"; last_name="Malone"; gpa=5.6 }

let get_name student = student.first_name,student.last_name

let create_student first_name last_name gpa =
    {first_name; last_name; gpa}
