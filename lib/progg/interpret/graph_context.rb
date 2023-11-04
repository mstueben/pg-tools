
module Progg
    module Interpret

        class GraphContext

            # The list of currently declared components
            attr_accessor :components

            attr_accessor :specs
    
            def initialize()
                @components = []
                @specs = []
            end
    
            # DSL method for declaring a new component in this graph
            def component(name, &blk)
                cmp = ComponentContext.new(name, self)
                cmp.instance_eval(&blk)
                @components << cmp
                return cmp
            end

            def transient(name)
                cmp = component(name) do
                    all_states = [ :off, :on ]
                    states(*all_states)
                    all_states.product(all_states).each { |s1, s2| transition({ s1 => s2}) }
                end
                cmp.represents_fault = true
                return cmp
            end

            def error(name)
                return name
            end

            def no_errors()
                return @components.select(&:represents_fault?).map { |cmp|
                    "#{cmp.name} == no"
                }.join(' && ')
            end

            # DSL method for declaring a new specification in this graph
            def specify(text, &blk)
                specset = SpecSetContext.new(text, nil)
                specset.instance_eval(&blk)
                @specs << specset
            end

            def to_model()
                components = @components.map(&:to_model)
                variables = Model::VariableSet.new(*@components.map(&:owned_variables).flatten())
                specification = Model::Specification.new(@specs.map { |s| s.to_model(nil) })
                Model::Graph.new(components: components, variables: variables, specification: specification)
            end
    
            def get_binding()
                binding()
            end
    
        end

    end
end
