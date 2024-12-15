import java.io.BufferedReader;
import java.io.FileReader;

public class Solution {
	private static final String INPUT_FILE = "./inputs/d00.txt"; // Modify here and remove this comment
	private static String example;
	static {
		var tmp = new StringBuilder();
		//tmp.append("line1\n");
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
		} catch (Exception e) { }
	}

	void solvePart1(String input) {

	}

	void solvePart2(String input) {

	}
}
