module PgVerify
    module Model

        class Trace

            attr_accessor :model
            attr_accessor :states
            attr_accessor :loop_index

            def initialize(model, states, loop_index: -1)
                @model, @states = model, states
                @loop_index = loop_index
            end

            # TODO: not sure whether I can use paramter with to_s?
            def to_s(include_steps: true, hide_faults: true)
                return "No states in trace" if @states.empty?
                puts hide_faults
                # Get all variables (TODO: Bring into sensible order)
                vars = @states.first.keys
                state_vars = @model.state_variables()
                fault_state_vars = @model.fault_state_variables()

                parts = vars.map { |var| 
                  if (fault_state_vars.varname?(var) && hide_faults) then "" else 
                    var_string = state_vars.varname?(var) ? var.to_s.c_state.c_bold : var.to_s.c_string
                    var_string + "\n" + @states.each_with_index.map{ |state, index| value_str(var, state[var], index) }.join("\n")
                  end
                }
                str = "Step".c_num.c_bold + "\n" + (0...@states.length).map { |i| "#{i + 1}" } .join("\n")
                str = "" unless include_steps
                parts.each { |part| str = str.line_combine(part, separator: "  ") }

                unless @loop_index.nil? || @loop_index < 0
                    loop_str = (0...@states.length).map { |i| i == @loop_index + 1 ? "<- loop".c_blue : "" } .join("\n")
                    str = str.line_combine(loop_str, separator: "  ")
                    str += "\n" + "Loop starts at #{@loop_index + 1}".c_sidenote
                end
                
                return str
            end

            def value_str(key, value, index)
                return value.c_green if [ "On", "Yes", "Active" ].include?(value)
                return value.c_red if [ "Off", "No", "Idle" ].include?(value)


                settings_color = Settings.trace.colors.find { |key, val| File.fnmatch?(key.to_s, value) }
                settings_color = settings_color[1] unless settings_color.blank?
                return value.send(:"c_#{settings_color}") unless settings_color.blank?

                return "#{value}" 

            end

        end

    end
end
