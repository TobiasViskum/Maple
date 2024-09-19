Tobias := module()
    description "Mine Maple-kommandoer";
    option package;
    export monotoniforhold;

    monotoniforhold := proc(f, range := x=-infinity..infinity)
        local i::posint := 0;
        local solutions::list := sort([fsolve([diff(f, x) = 0])]);
        local solution::realcons;
        local next_solution::realcons;
        local intervaller::list := [];
        local interval::string;

        local interval_fmt_string::string := "%s%s; %s]";
        local mon_string::string;
        local final_string::string := "\n";

        local lower_bound::realcons := lhs(rhs(range));
        local upper_bound::realcons := rhs(rhs(range));

        # Special behavior: Floats w/ 3 decimals, infinite display
        local stringify := proc(val::realcons)
            if convert(val, string) = "infinity" then
                return "&infin;";
            elif convert(val, string) = "-infinity" then
                return "-&infin;";
            else
                return sprintf("%.3f", val);
            end if;
        end proc;

        # kind = "max" | "min"
        local pick := proc(kind::string, l::realcons, r::realcons)
            local val::realcons;
            if kind = "max" then
                val := max(l, r);
            else
                val := min(l, r);    
            end if;
            return stringify(val);
        end proc:

        # Kind = "down" | "up"
        local get_mon_string := proc(kind::string, x_val::realcons)
            local offset_x_val::realcons, hældning::realcons;
            if kind = "down" then
                offset_x_val := x_val - 0.1;
            else
                offset_x_val := x_val + 0.1;
            end if;

            hældning := subs(x = offset_x_val, diff(f, x));

            if hældning > 0 then
                return "Voksende i [";
            else
                return "Aftagende i [";
            end if;
        end proc:

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
                push_interval(sprintf(
                    interval_fmt_string,
                    mon_string,
                    pick("max", lower_bound, -infinity),
                    pick("min", upper_bound, solution)
                ));
            end if;

            mon_string := get_mon_string("up", solution);
            if nops(solutions) = i + 1 then
                push_interval(sprintf(
                    interval_fmt_string,
                    mon_string,
                    pick("max", lower_bound, solution),
                    pick("min", upper_bound, infinity)
                ));
            else
                next_solution := rhs(solutions[i + 2][1]);
                push_interval(sprintf(
                    interval_fmt_string,
                    mon_string,
                    pick("max", lower_bound, solution),
                    pick("min", upper_bound, next_solution)
                ))
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
                stringify(lower_bound),
                stringify(upper_bound)
            ));
        end if;

        # Builds the output string
        for interval in intervaller do
            final_string := sprintf("%s%s\n", final_string, interval);
        end do;

        return final_string;
    end proc;
end module: