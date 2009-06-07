#1/usr/bin/ruby -w

class GattacaFormat

  def self.parse_predictions data
    lines = data.split /[\r\n]+/

    base_pair_count = lines.first.to_i
    prediction_count_line_number = (base_pair_count / 80.0).ceil + 1
    prediction_count = lines[prediction_count_line_number].to_i

    lines[(prediction_count_line_number+1), prediction_count].map do |line|
      line.split(/\s+/).map {|datum| datum.to_i }
    end.sort do |a, b|
      a[1] <=> b[1]
    end
  end

end

class GattacaSolver

  def initialize filename
    @predictions = GattacaFormat.parse_predictions File.read(filename)
    @maximums = Array.new
  end

  def run
    first_start, first_stop, first_score = @predictions.shift

    @maximums << [first_stop, first_score]
    last_end_point = first_stop

    @predictions.each do |prediction|
      start, stop, score = prediction
      attempt = score + best_less_than(start)

      if attempt > @maximums.first.last
        if stop == last_end_point
          @maximums.first.last = attempt
        else
          @maximums.unshift [stop, attempt]
        end
      end
    end

    @maximums.first.last
  end

  def best_less_than n
    best = @maximums.detect {|point, max| point < n }
    best ? best.last : 0
  end

end

p GattacaSolver.new(ARGV[0]).run
