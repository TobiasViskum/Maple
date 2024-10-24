Tobias := module()
    description "Tobias' Maple-kommandoer";
    option package;
    export monotoniforhold, bestemtTangentIPunkt;

    bestemtTangentIPunkt := proc(diffEq, punkt)
        local xcoord::realcons := op(1, punkt);
        local ycoord::realcons := op(2, punkt);

        # Den fuldstændige løsning
        local sol := dsolve(diffEq);

        # Navnet på funktionen givet f.eks. `y` i `y(x)`
        local fnName := op(0, lhs(sol));

        # Navnet på den afhængige variabel f.eks. `x` eller `t`
        local dependantVar := op(1, lhs(sol));

        # Den partikulære løsning
        local fn := unapply(rhs(dsolve({diffEq, fnName(xcoord) = ycoord})), dependantVar);

        # Hældningen i punktet x0
        local hældning := eval(diff(fn(dependantVar), dependantVar), dependantVar = xcoord);

        # Tangentligningen
        local tangent := hældning * (x - xcoord) + fn(xcoord);

        return y = tangent;
    end proc;    

    monotoniforhold := proc(f, range := x=-infinity..infinity)
        local i::posint := 0;
        local solutions::list := sort([fsolve([diff(f, x) = 0])]);
        local solution::realcons;
        local next_solution::realcons;
        local intervaller::list := [];
        local interval::string;

        local interval_fmt_string::string := "%s%s%s; %s%s";
        local mon_string::string;
        local final_string::string := "\n";

        local lower_bound::realcons := lhs(rhs(range));
        local upper_bound::realcons := rhs(rhs(range));

        local picked_lower_bound::realcons;
        local picked_upper_bound::realcons;

        # Returns: "float" | "posinf" | "neginf"
        local get_val_kind := proc(val::realcons)
            if convert(val, string) = "infinity" then
                return "posinf";
            elif convert(val, string) = "-infinity" then
                return "neginf";
            else
                return "float";
            end if;
        end proc;

        # Behavior: Displays floats with 3 decimals and infinite as symbols
        local stringify := proc(val::realcons)
            local inf_kind := get_val_kind(val);
            if inf_kind = "posinf" then
                return "&infin;";
            elif inf_kind = "neginf" then
                return "-&infin;";
            else
                return sprintf("%.3f", val);
            end if;
        end proc;

        # Kind = "upper" | "lower"
        # Returns: "[" | "]"
        local get_bracket_kind := proc(kind::string, val::realcons)
            local inf_kind::string := get_val_kind(val);

            if kind = "upper" then
                if inf_kind = "float" then
                    return "]";
                else
                    return "[";
                end if;
            else
                if inf_kind = "float" then
                    return "[";
                else
                    return "]";
                end if;
            end if;
        end proc;

        # Kind = "down" | "up"
        local get_mon_string := proc(kind::string, x_val::realcons)
            local offset_x_val::realcons, hældning::realcons;
            if kind = "down" then
                offset_x_val := x_val - 0.1;
            else
                offset_x_val := x_val + 0.1;
            end if;

            hældning := evalf(subs(x = offset_x_val, diff(f, x)));

            if hældning > 0 then
                return "Voksende i ";
            else
                return "Aftagende i ";
            end if;
        end proc;

        # Pushes an interval to the list `intervaller`
        local push_interval := proc(item::string)
            intervaller := [op(intervaller), item];
        end proc;

        # Main loop
        for solution in solutions do
            solution := rhs(solution[1]);

            if solution < lower_bound or solution > upper_bound then
                next;
            end if;

            if i = 0 then
                mon_string := get_mon_string("down", solution);
                picked_lower_bound := max(lower_bound, -infinity);
                picked_upper_bound := min(upper_bound, solution);

                push_interval(sprintf(
                    interval_fmt_string,
                    mon_string,
                    get_bracket_kind("lower", picked_lower_bound),
                    stringify(picked_lower_bound),
                    stringify(picked_upper_bound),
                    get_bracket_kind("upper", picked_lower_bound)
                ));
            end if;

            mon_string := get_mon_string("up", solution);

            if nops(solutions) = i + 1 then
                picked_lower_bound := max(lower_bound, solution);
                picked_upper_bound := min(upper_bound, infinity);
                
                push_interval(sprintf(
                    interval_fmt_string,
                    mon_string,
                    get_bracket_kind("lower", picked_lower_bound),
                    stringify(picked_lower_bound),
                    stringify(picked_upper_bound),
                    get_bracket_kind("upper", picked_upper_bound)
                ));
            else
                next_solution := rhs(solutions[i + 2][1]);
                picked_lower_bound := max(lower_bound, solution);
                picked_upper_bound := min(upper_bound, next_solution);

                push_interval(sprintf(
                    interval_fmt_string,
                    mon_string,
                    get_bracket_kind("lower", picked_lower_bound),
                    stringify(picked_lower_bound),
                    stringify(picked_upper_bound),
                    get_bracket_kind("upper", picked_upper_bound)
                ));
            end if;

            i := i + 1;
        end do;

        # If there are no intervals (due to range)
        # then there must be at least one interval present
        if nops(intervaller) = 0 then
            mon_string := get_mon_string("up", lower_bound);


            push_interval(sprintf(
                interval_fmt_string,
                mon_string,
                get_bracket_kind("lower", lower_bound),
                stringify(lower_bound),
                stringify(upper_bound),
                get_bracket_kind("upper", upper_bound)
            ));
        end if;

        # Builds the output string
        for interval in intervaller do
            final_string := sprintf("%s%s\n", final_string, interval);
        end do;

        return final_string;
    end proc;  
end module: