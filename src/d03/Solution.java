import java.io.BufferedReader;
import java.io.FileReader;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Solution {
	private static final String INPUT_FILE = "./inputs/d02.txt";
	private static String example;
	static {
		var tmp = new StringBuilder();
		tmp.append("7 6 4 2 1\n");
		tmp.append("1 2 7 8 9\n");
		tmp.append("9 7 6 2 1\n");
		tmp.append("1 3 2 4 5\n");
		tmp.append("8 6 4 4 1\n");
		tmp.append("1 3 6 7 9\n");
		example = tmp.toString();
	}
	public static void main(String[] args) {
		var sol = new Solution();

		sol.solvePart1(example);
		sol.solvePart2(example);

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
		}
	}

	void solvePart1(String input) {
		var r = input.lines()
			.map(l -> {
				String[] levelsStr = l.split(" ");
				List<Integer> levels = new LinkedList<>();
				for (String s : levelsStr) {
					levels.add(Integer.parseInt(s));
				};

				List<Integer> deviation = new LinkedList<>();
				for (int i = 1; i < levels.size(); i++) {
					var dev = levels.get(i) - levels.get(i - 1);
					deviation.add(dev);
				};

				return ( deviation.stream().allMatch(i -> i >= 0) ||
						deviation.stream().allMatch(i -> i <= 0) ) &&
					deviation.stream().map(Math::abs).allMatch(i -> i >= 1 && i <= 3);
			})
			.filter(b -> b)
			.count();
			// .collect(Collectors.toList());
		System.out.println(r);
	}

	void solvePart2(String input) {
		var r = input.lines()
			.map(l -> {
				String[] levelsStr = l.split(" ");
				List<Integer> levels = new LinkedList<>();
				for (String s : levelsStr) {
					levels.add(Integer.parseInt(s));
				};

				List<List<Integer>> tolerance = new LinkedList<>();
				tolerance.add(levels);

				for (int i = 0; i < levels.size(); i++) {
					List<Integer> cln = new LinkedList<>();
					cln.addAll(levels);
					cln.remove(i);
					tolerance.add(cln);
				}
				levels = null;

				boolean result = false;
				for (List<Integer> lev : tolerance) {
					List<Integer> deviation = new LinkedList<>();
					for (int i = 1; i < lev.size(); i++) {
						var dev = lev.get(i) - lev.get(i - 1);
						deviation.add(dev);
					};

					result |= ( deviation.stream().allMatch(i -> i >= 0) ||
							deviation.stream().allMatch(i -> i <= 0) ) &&
						deviation.stream().map(Math::abs).allMatch(i -> i >= 1 && i <= 3);
					if (result) break;
				}
				return result;
			})
			.filter(b -> b)
			.count();
			// .collect(Collectors.toList());
		System.out.println(r);
	}
}
