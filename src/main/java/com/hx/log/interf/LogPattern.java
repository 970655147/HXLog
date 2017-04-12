/**
 * file name : LogPattern.java
 * created at : 11:37:47 PM Apr 21, 2016
 * created by 970655147
 */

package com.hx.log.interf;

// ��־ģʽ�Ľӿ�, ��������
public interface LogPattern {

	/**
	 * ��ȡ��ǰ��pattern�Ľ����Ϣ
	 *
	 * @return the result of current LogPattern
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:20 PM
	 * @since 1.0
	 */
	String pattern();

	/**
	 * ��ȡ��ǰ��pattern������
	 *
	 * @return current LogPattern's LogPatternType
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:20 PM
	 * @since 1.0
	 */
	LogPatternType type();

	/**
	 * ��ȡ��ǰ��pattern�Ŀ�������
	 *
	 * @return an copy of current LogPattern
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:20 PM
	 * @since 1.0
	 */
	<T extends LogPattern> T copyOf();


	/**
	 * һ������������LogPattern
	 *
	 * @author Jerry.X.He <970655147@qq.com>
	 * @version 1.0
	 * @date 4/12/2017 10:21 PM
	 */
	abstract class OneStringVariableLogPattern implements LogPattern {
		protected String arg;

		public OneStringVariableLogPattern(String arg) {
			setArg(arg);
		}

		/**
		 * ���ò���, �Լ�Ĭ�ϵ�patternʵ��
		 *
		 * @return
		 * @author Jerry.X.He
		 * @date 4/12/2017 10:20 PM
		 * @since 1.0
		 */
		public void setArg(String arg) {
			this.arg = arg;
		}

		@Override
		public String pattern() {
			return arg;
		}
		public LogPattern copyOf() {
			return this;
		}
	}
	
}

