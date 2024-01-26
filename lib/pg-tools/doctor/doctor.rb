module PgTools
    module Doctor

        Warning = Struct.new(:title, :text)

        class DoctorError < PgTools::Core::Error
            def initialize(warnings)
                @warnings = warnings
            end
            def formatted()
                is_are, s = @warnings.length == 1 ? ["is", ""] : ["are", "s"]
                header = "There #{is_are} #{@warnings.length} warning#{s} for your installation:"
                string = @warnings.each_with_index.map { |w, i|
                    title = "#{i + 1}) #{w.title}".c_warn
                    body  = w.text.indented(str: "   ")
                    "#{title}\n#{body}"
                }.join("\n\n")
                return header, string
            end
        end

        def self.check()
            checks = Doctor.methods
                .select { |method| method.to_s.start_with?("check_") }
                .map { |sym| sym.to_s.sub("check_", "").to_sym }
                .sort
            warnings = checks.map { |check| run_check(check) }.flatten.compact
            raise DoctorError.new(warnings) unless warnings.empty?
        end

        def self.run_check(symbol)
            Shell::LoadingPrompt.while_loading("Checking #{symbol.to_s.gsub('_', ' ').c_string}") {
                warnings = ([self.send(:"check_#{symbol}")] || []).flatten.compact
                state = warnings.empty? ? :success : :error
                msg   = warnings.empty? ? "Ok" : "#{warnings.length} warning(s)!"
                # Allow returning :skip from the check to mark the check as skipped
                if warnings.length == 1 && warnings.first == :skip
                    state, msg, warnings = :empty, "Skipped!", []
                end
                Shell::LoadingPrompt::LoadingResult.new(warnings, msg, state: state)
            }
        end

        def self.check_01_Can_find_NuSMV()
            return [] unless PgTools::NuSMV::Runner.new.find_nusmv_path.nil?
            return Warning.new("Unable to locate the NuSMV executable", 
                "Make sure to install NuSMV by unpacking it and placing the entire folder into\n" \
                "the #{'addon'.c_file} directory of your project. " \
                "(#{File.expand_path('addon').c_sidenote})\n" \
                "Alternatively you can set the #{'numsv.path'.c_string} in the configuration."
            )
        end

        def self.check_02_Can_run_NuSMV()
            path = PgTools::NuSMV::Runner.new.find_nusmv_path
            return :skip if path.nil?

            # Test by evaluating some example smv file
            example_file = File.join(PgTools.root, "data", "nusmv.sample.smv")
            return [] if Core::CMDRunner.run_for_exit_code("#{path} #{example_file}") == 0

            return Warning.new("Unable to run the NuSMV executable", 
                "NuSMV could be found here: #{path.c_file}\n" \
                "However it could not be executed. Here are a few things to try:\n" \
                " - Make sure the file is executable\n" \
                " - Make sure you have the required permissions"
            )
        end

        def self.check_03_Run_integration_tests()
            return :skip if PgTools::NuSMV::Runner.new.find_nusmv_path.nil?

            warnings = []

            test_files = Dir[File.join(PgTools.root, "integration_tests", "ruby_dsl", "*.rb")].sort
            warnings = test_files.map { |test_file|
                model = Interpret::PgScript.new.interpret(test_file).first
                PgTools::Model::Validation.validate!(model)
                results = NuSMV::Runner.new().run_specs(model)
                failures = results.reject(&:success)
                next if failures.empty?

                test_name = File.basename(test_file, '.*').gsub("_", "-")
                failures_s = failures.map { |f| "#{f.spec.text} (#{f.spec.expression.to_s.c_blue})" }
                show_command = "$ pg-tools show nusmv --script #{File.expand_path(test_file)}".c_cyan
                test_command = "$ pg-tools test --script #{File.expand_path(test_file)}".c_cyan
                Warning.new("Failed integration test in #{test_name}", 
                   "The test #{test_name.c_string} contains the following unsatisfied specifications:\n" \
                   "\t- #{failures_s.join("\n\t- ")}\n" \
                   "These specifications should be valid if pg-tools works as expected.\n" \
                   "You can use the following commands to debug this:\n" \
                   "  #{show_command}\n  #{test_command}"
                )

            }.compact
            return warnings
        end

        # def self.check_03_Can_find_PlantUML()
        #     return [] unless PgTools::Puml.find_path.nil?
        #     return Warning.new("Unable to find the PlantUML executable", 
        #         "Make sure to install PlantUML by dropping the jar into\n" \
        #         "the #{'addon'.c_file} directory of your project. " \
        #         "(#{File.expand_path('addon').c_sidenote})\n" \
        #         "You can get it from here: #{"https://plantuml.com/download".c_string}\n" \
        #         "PG-Tools will fall back to using the PlantUML web-server otherwise."
        #     )
        # end

        # def self.check_04_Can_run_PlantUML()
        #     path = PgTools::Puml.find_path
        #     return :skip if path.blank?

        #     return [] if Core::CMDRunner.run_for_exit_code("java -jar #{path} -help") == 0
        #     puts "java -jar #{path} -help"

        #     return Warning.new("Unable to run the PlantUML executable",
        #         "PlantUML could be found here: #{path.c_file}\n" \
        #         "However it could not be executed. Here are a few things to try:\n" \
        #         " - Make sure the file is executable\n" \
        #         " - Make sure you have the required permissions"
        #     )
        # end

    end
end

# Require all module files
Dir[File.join(__dir__, "**", '*.rb')].sort.each { |file| require file }