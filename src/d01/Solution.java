import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Solution {
	private static final String INPUT_FILE = "./inputs/d01.txt";
	private static String example;
	static {
		var tmp = new StringBuilder();
		tmp.append("3   4\n");
		tmp.append("4   3\n");
		tmp.append("2   5\n");
		tmp.append("1   3\n");
		tmp.append("3   9\n");
		tmp.append("3   3");
		example = tmp.toString();
	}
	public static void main(String[] args) {
		var sol = new Solution();

		//sol.solvePart1(example);
		//sol.solvePart2(example);

		try {
			var strb = new StringBuilder();
			var bufreader = new BufferedReader(new FileReader(INPUT_FILE));
			String tmp;
			while (true) {
				tmp = bufreader.readLine();
				if (tmp != null) {
					strb.append(tmp + "\n");
				} else {
					bufreader.close();
					break;
				}
			}
			sol.solvePart1(strb.toString());
			sol.solvePart2(strb.toString());
		} catch (Exception e) {
			e.printStackTrace();
			System.err.println("File not found");
		}
	}

	final Collector<
		List<Integer>,
		List<List<Integer>>,
		List<List<Integer>>
			> myCollector = new Collector<>() {

				@Override
				public Supplier<List<List<Integer>>> supplier() {
					return LinkedList::new;
				}

				@Override
				public BiConsumer<List<List<Integer>>, List<Integer>> accumulator() {
					return (acc, val) -> {
						for (int i = 0; i < val.size(); i++) {
							if (acc.size() <= i) {
								acc.add(new ArrayList<>());
							}
							acc.get(i).add(val.get(i));
						}
					};
				}

				@Override
				public BinaryOperator<List<List<Integer>>> combiner() {
					return (a, b) -> {
						a.addAll(b);
						return a;
					};
				}

				@Override
				public Function<List<List<Integer>>, List<List<Integer>>> finisher() {
					return r -> {
						r.forEach(Collections::sort);
						return r;
					};
				}

				@Override
				public Set<Characteristics> characteristics() {
					return Set.of(Characteristics.UNORDERED);
				}
			};

	void solvePart1(String input) {
		var parsed = input
			.lines()
			.map(l -> {
				var p = Pattern.compile("(\\d+)\\s+(\\d+)");
				var m = p.matcher(l);
				m.matches();
				return List.of(
						Integer.parseInt(m.group(1)),
						Integer.parseInt(m.group(2))
						);
			})
		.collect(myCollector);

		var processed = IntStream.range(0, parsed.get(0).size())
			.mapToObj(i -> parsed.stream()
					.map(l -> l.get(i))
					.collect(Collectors.toList())
					)
			.map(l -> Math.abs(l.get(0) - l.get(1)))
			.reduce(Integer::sum);

		System.out.println(processed);
	}

	void solvePart2(String input) {
		var parsed = input
			.lines()
			.map(l -> {
				var p = Pattern.compile("(\\d+)\\s+(\\d+)");
				var m = p.matcher(l);
				m.matches();
				return List.of(
						Integer.parseInt(m.group(1)),
						Integer.parseInt(m.group(2))
						);
			})
			.collect(Collectors.toList());

		List<Integer> l1 = new LinkedList<Integer>(),
			l2 = new LinkedList<Integer>();

		parsed.forEach(l -> {
			l1.add(l.get(0));
			l2.add(l.get(1));
		});

		var processed = l1.stream()
			.map(i -> {
				return (l2.stream().filter(j -> i.equals(j))
					.count()) * i;
			})
			.reduce(Long::sum);


		System.out.println(processed);
	}
}
