/**
 * file name : Test01JsonExtractor.java
 * created at : ����6:22:03 2016��8��11��
 * created by 970655147
 */

package com.hx.log.test;

import static com.hx.log.util.Log.info;
import static com.hx.log.util.Log.log;

import java.util.BitSet;

import org.junit.Test;

import com.hx.log.util.Eval;
import com.hx.log.util.interf.IdxIterator;
import com.hx.log.util.interf.IdxIterator.SomeBitIncIdxIterator;
import com.hx.log.util.JSONExtractor;

import net.sf.json.JSONArray;
import net.sf.json.JSONObject;

public class Test01_JsonExtractor {

	// ���ݸ����ı��ʽ, ��ȡ�����Ķ����е�����
	public static void main(String[] args) {
	
		
	}
	
	@Test
	public void testEval() {
		// test ;
//		log(Eval.eval("abc") );
//		log(Eval.eval("??2+12") );
		log(Eval.eval("1 + ((1 * 5)) + 5") );
	}
	
	@Test
	public void testPrepare() {
//		String str = "????";
		String str = "????*";
//		String str = "????**";
//		String str = "????**12*";
		log(JSONExtractor.preparePattern(str) );
	}
	
	@Test 
	public void testIdxIterator() {
////	IdxIterator ite = new SingleIdxIterator(2);
////	IdxIterator ite = new RangeIdxIterator(2, 12);
////	IdxIterator ite = Inc.newCntInc(1, 2, 3);
//		IdxIterator ite = Inc.newEndInc(1, 2, 9);
		BitSet bs = new BitSet();
			bs.set(0);
			bs.set(2);
		IdxIterator ite = new SomeBitIncIdxIterator(20, 120, bs);
		while(ite.hasNext() ) {
			log(ite.next() );
		}
	}
	
	@Test
	public void getIdxIteratorByPattern0() {
		IdxIterator ite = JSONExtractor.getIdxIteratorByPattern("???2", 123);
		while(ite.hasNext() ) {
			log(ite.next() );
		}
	}
	
	@Test
	public void testExtractJson() {
		
//		String pattern = "$this[1, 4].category.url";
//		String pattern = "$this['?2?', '?3?'].category.url";
//		String pattern = "$this[?2?, ?3?].category.url";
//		String pattern = "$this.category[*].url";
//		String pattern = "$this.category[0,2]";
//		JSONObject obj = new JSONObject().element("category", new JSONArray().element(new JSONObject().element("url", "http://www.abidu.com")).element(new JSONObject().element("url", "http://www.hx.com")) );
		
//		String pattern = "$this.page-1[*].products[0, 10].title";
//		String pattern = "$this.page-1[*].products[1, ?].title";
//		String pattern = "$this.page-1[*].products[??].title";
//		JSONObject obj = JSONObject.fromObject("{\"page-1\":[{\"products\":[{\"title\":\"08 �����߳̽����ӡ121212...\",\"date\":\"2016-08-09 21:43\",\"view\":\"(8)\"},{\"title\":\"17 FileNameMatcher\",\"date\":\"2016-08-07 21:20\",\"view\":\"(12)\"},{\"title\":\"16 pointFixLike\",\"date\":\"2016-05-21 22:00\",\"view\":\"(1841)\"},{\"title\":\"һ�����˵��۵���\",\"date\":\"2016-04-09 23:08\",\"view\":\"(89)\"},{\"title\":\"15 ����Calender��صķ���\",\"date\":\"2016-03-03 21:48\",\"view\":\"(103)\"},{\"title\":\"08 scala, imported `Record' is permanently hidden by definition of class Record in package test\",\"date\":\"2016-03-01 20:57\",\"view\":\"(161)\"},{\"title\":\"01 SparkStreaming's WordCount\",\"date\":\"2016-02-12 21:49\",\"view\":\"(211)\"},{\"title\":\"14 screenShotLikeQQ\",\"date\":\"2016-02-02 20:53\",\"view\":\"(121)\"},{\"title\":\"13 gifGenerator\",\"date\":\"2016-02-01 20:36\",\"view\":\"(146)\"},{\"title\":\"12 ���ˮӡ\",\"date\":\"2016-01-27 21:08\",\"view\":\"(100)\"},{\"title\":\"11 ����ѩ����̬ͼ\",\"date\":\"2016-01-26 20:31\",\"view\":\"(102)\"},{\"title\":\"07 �˻ʺ�����\",\"date\":\"2016-01-26 20:08\",\"view\":\"(104)\"},{\"title\":\"10 ��������\",\"date\":\"2016-01-25 21:05\",\"view\":\"(92)\"},{\"title\":\"30 ��n�����������ȡm������\",\"date\":\"2016-01-24 20:44\",\"view\":\"(387)\"},{\"title\":\"29 ͬλ�ʵ�ͳ��\",\"date\":\"2016-01-23 20:33\",\"view\":\"(89)\"}]}],\"page-2\":[{\"products\":[{\"title\":\"28 �ҳ������ڵ�����\",\"date\":\"2016-01-23 10:21\",\"view\":\"(107)\"},{\"title\":\"27 �绰��������\",\"date\":\"2016-01-20 21:22\",\"view\":\"(102)\"},{\"title\":\"26 parseInt\",\"date\":\"2016-01-20 20:24\",\"view\":\"(97)\"},{\"title\":\"25 ��ʹ�üӼ��˳����ӷ�\",\"date\":\"2016-01-18 21:08\",\"view\":\"(84)\"},{\"title\":\"24 Լɪ��\",\"date\":\"2016-01-17 20:51\",\"view\":\"(84)\"},{\"title\":\"23 �ж��˿��Ƶ�˳��\",\"date\":\"2016-01-17 20:42\",\"view\":\"(73)\"},{\"title\":\"22 k����������n�ĸ���\",\"date\":\"2016-01-16 21:03\",\"view\":\"(85)\"},{\"title\":\"21 ��ת����˳��\",\"date\":\"2016-01-16 20:32\",\"view\":\"(88)\"},{\"title\":\"20 �ҳ���1500������\",\"date\":\"2016-01-15 21:09\",\"view\":\"(73)\"},{\"title\":\"19 ��ȡ�����������ܹ���ɵ���С������\",\"date\":\"2016-01-15 10:47\",\"view\":\"(73)\"},{\"title\":\"18 ��ȡ���������е���������, ���\",\"date\":\"2016-01-15 10:00\",\"view\":\"(81)\"},{\"title\":\"17 ������������ת��Ϊ����˫����\",\"date\":\"2016-01-12 21:11\",\"view\":\"(72)\"},{\"title\":\"16 ��������ĸ���\",\"date\":\"2016-01-12 20:55\",\"view\":\"(73)\"},{\"title\":\"15 ��ӡ�����Ķ������к�Ϊ������ֵ��·��\",\"date\":\"2016-01-11 21:06\",\"view\":\"(66)\"},{\"title\":\"14 �жϸ����Ķ���������������������Ƿ�Ϸ�\",\"date\":\"2016-01-11 20:45\",\"view\":\"(65)\"}]}],\"page-3\":[{\"products\":[{\"title\":\"13 �����ĳ�ջ�����Ƿ�������ջ����\",\"date\":\"2016-01-09 21:19\",\"view\":\"(80)\"},{\"title\":\"12 ���δ�ӡ�����е�����\",\"date\":\"2016-01-09 21:01\",\"view\":\"(89)\"},{\"title\":\"11 �Ƿ�������� & ����������\",\"date\":\"2016-01-08 20:33\",\"view\":\"(90)\"},{\"title\":\"10 �鲢������������\",\"date\":\"2016-01-08 19:47\",\"view\":\"(91)\"},{\"title\":\"09 �������ҳ�������k���� & �ҳ��������м������\",\"date\":\"2016-01-01 21:05\",\"view\":\"(72)\"},{\"title\":\"08 ����������ż��֮ǰ\",\"date\":\"2015-12-31 21:18\",\"view\":\"(66)\"},{\"title\":\"07 ��ӡ1������nλ��\",\"date\":\"2015-12-31 20:59\",\"view\":\"(98)\"},{\"title\":\"06 ʵ����ֵ�������η�\",\"date\":\"2015-12-30 21:21\",\"view\":\"(72)\"},{\"title\":\"05 ��ת�����е���С����\",\"date\":\"2015-12-30 20:20\",\"view\":\"(65)\"},{\"title\":\"04 ����ջ������� & �������й���ջ\",\"date\":\"2015-12-29 21:26\",\"view\":\"(69)\"},{\"title\":\"03 ��������ͷ���, ���������β�ڵ㵽ͷ��������\",\"date\":\"2015-12-29 20:42\",\"view\":\"(72)\"},{\"title\":\"02 �滻' 'Ϊ\\\"%32\\\"\",\"date\":\"2015-12-28 22:46\",\"view\":\"(57)\"},{\"title\":\"01 �ڵ���[�����<�����, ��ߵ�<�ұߵ�]��ά�����в��Ҹ�����ֵ\",\"date\":\"2015-12-28 21:50\",\"view\":\"(80)\"},{\"title\":\"06 RedBlackTree\",\"date\":\"2015-12-28 20:10\",\"view\":\"(73)\"},{\"title\":\"07 eclipse C/ C++���뺬�ж��main��������Ŀ\",\"date\":\"2015-12-23 21:50\",\"view\":\"(194)\"}]}],\"page-4\":[{\"products\":[{\"title\":\"09 DirectoryStructure\",\"date\":\"2015-12-21 21:26\",\"view\":\"(91)\"},{\"title\":\"06 ���ξ���\",\"date\":\"2015-12-14 22:23\",\"view\":\"(132)\"},{\"title\":\"05 ���׼�����\",\"date\":\"2015-12-14 22:11\",\"view\":\"(71)\"},{\"title\":\"04 ����ƽ̨��\",\"date\":\"2015-12-14 22:08\",\"view\":\"(58)\"},{\"title\":\"05 BinarySortTree\",\"date\":\"2015-12-14 10:50\",\"view\":\"(76)\"},{\"title\":\"04 BloomFilter\",\"date\":\"2015-12-04 21:05\",\"view\":\"(95)\"},{\"title\":\"06 maven���� \\\"Could not calculate build plan: Plugin 'jarPrefix':'jarName':'jarVersion' \\\"\",\"date\":\"2015-12-03 21:16\",\"view\":\"(250)\"},{\"title\":\"02 ��һ��ʵϰ������ְ֮���������\",\"date\":\"2015-12-02 22:10\",\"view\":\"(101)\"},{\"title\":\"08 SimpleServer\",\"date\":\"2015-11-27 21:27\",\"view\":\"(124)\"},{\"title\":\"03 MyBitSet\",\"date\":\"2015-11-27 16:23\",\"view\":\"(92)\"},{\"title\":\"15 java.util.BitSet\",\"date\":\"2015-11-21 20:42\",\"view\":\"(108)\"},{\"title\":\"02 �����ַ������ʽ [������js eval����]\",\"date\":\"2015-11-21 15:43\",\"view\":\"(88)\"},{\"title\":\"07 java.awt.Robot��һЩʹ��\",\"date\":\"2015-11-18 20:57\",\"view\":\"(179)\"},{\"title\":\"0602 ���Ϳ�ܷ���\",\"date\":\"2015-11-16 17:31\",\"view\":\"(106)\"},{\"title\":\"0601 ������֤��\",\"date\":\"2015-11-16 13:28\",\"view\":\"(77)\"}]}],\"page-5\":[{\"products\":[{\"title\":\"10 2048\",\"date\":\"2015-11-15 21:58\",\"view\":\"(85)\"},{\"title\":\"05 ueditor�ϴ�ͼƬ����\",\"date\":\"2015-11-15 17:21\",\"view\":\"(90)\"},{\"title\":\"0103 �쵰���ĵ�[װ*����]\",\"date\":\"2015-11-12 12:34\",\"view\":\"(105)\"},{\"title\":\"0102 �쵰���ĵ�[���˷�λ(��ͼ, ����)]\",\"date\":\"2015-11-11 22:56\",\"view\":\"(93)\"},{\"title\":\"0101 �쵰���ĵ�[����Ԫ��]\",\"date\":\"2015-11-11 20:30\",\"view\":\"(100)\"},{\"title\":\"06 �������һ������վ��, ����û�µ�ʱ��'���'һ��\",\"date\":\"2015-11-10 16:02\",\"view\":\"(256)\"},{\"title\":\"04 bootstrap \\\"ģ̬����ת����ǰģ��ҳ��, ����ʧ��, ��������Ȼ��������\\\"\",\"date\":\"2015-11-10 14:40\",\"view\":\"(339)\"},{\"title\":\"03 ������Ŀ��tomcat����.javaδ��������\",\"date\":\"2015-11-04 16:50\",\"view\":\"(126)\"},{\"title\":\"02 bootstrap \\\"modal : is not a function\\\"\",\"date\":\"2015-11-03 13:30\",\"view\":\"(422)\"},{\"title\":\"09 �ǻ���/ ��Ħ��\",\"date\":\"2015-10-28 20:58\",\"view\":\"(190)\"},{\"title\":\"01 ɸѡ��������\",\"date\":\"2015-10-25 22:14\",\"view\":\"(117)\"},{\"title\":\"01 �����������\",\"date\":\"2015-10-24 19:03\",\"view\":\"(84)\"},{\"title\":\"05 ����Ƭ��ʽ��\",\"date\":\"2015-10-22 21:25\",\"view\":\"(123)\"},{\"title\":\"04 eclipse �Զ���ȫ���ܵĸĽ�\",\"date\":\"2015-10-14 10:58\",\"view\":\"(122)\"},{\"title\":\"14 java.util.LinkedHashMap\",\"date\":\"2015-10-07 21:50\",\"view\":\"(134)\"}]}],\"page-6\":[{\"products\":[{\"title\":\"13 java.util.HashMap\",\"date\":\"2015-10-06 19:41\",\"view\":\"(112)\"},{\"title\":\"12 java.util.LinkedList\",\"date\":\"2015-10-05 19:29\",\"view\":\"(133)\"},{\"title\":\"11 java.util.ArrayList\",\"date\":\"2015-10-04 17:24\",\"view\":\"(169)\"},{\"title\":\"03crawler02 ��ȡ��������, ����ͼƬ��\",\"date\":\"2015-10-02 20:48\",\"view\":\"(141)\"},{\"title\":\"03crawler01 ��ȡֱ�����Ӿ��б�\",\"date\":\"2015-10-02 19:41\",\"view\":\"(113)\"},{\"title\":\"03 crawler\",\"date\":\"2015-10-02 16:21\",\"view\":\"(184)\"},{\"title\":\"02 transferTools\",\"date\":\"2015-10-01 19:28\",\"view\":\"(125)\"},{\"title\":\"01 ������������\",\"date\":\"2015-10-01 15:46\",\"view\":\"(96)\"},{\"title\":\"08 ������\",\"date\":\"2015-09-30 10:55\",\"view\":\"(118)\"},{\"title\":\"10 java.lang.ThreadLocal\",\"date\":\"2015-09-29 14:38\",\"view\":\"(119)\"},{\"title\":\"07 ɨ��\",\"date\":\"2015-09-28 16:19\",\"view\":\"(129)\"},{\"title\":\"4.9 ��������\",\"date\":\"2015-09-26 21:22\",\"view\":\"(122)\"},{\"title\":\"4.7 ������������\",\"date\":\"2015-09-18 22:40\",\"view\":\"(119)\"},{\"title\":\"09 java.lang.Runtime\",\"date\":\"2015-09-13 12:53\",\"view\":\"(108)\"},{\"title\":\"4.4 �����ĵ��Ƿ���������֮��\",\"date\":\"2015-09-10 21:49\",\"view\":\"(117)\"}]}],\"page-7\":[{\"products\":[{\"title\":\"3.8 �������н�����ľ��� & �ؽ������� & ˳�����������\",\"date\":\"2015-09-05 19:55\",\"view\":\"(125)\"},{\"title\":\"08 java.lang.ProcessBuilder\",\"date\":\"2015-08-29 16:27\",\"view\":\"(145)\"},{\"title\":\"3.7 ���е����ֵ����\",\"date\":\"2015-08-28 22:03\",\"view\":\"(122)\"},{\"title\":\"06 �ڰ���\",\"date\":\"2015-08-28 16:12\",\"view\":\"(127)\"},{\"title\":\"3.6 �ж������޻������Ƿ��ཻ & �ҳ��ཻ�ĵ�һ�����\",\"date\":\"2015-08-28 15:05\",\"view\":\"(116)\"},{\"title\":\"3.4 ����ͷ������ɾ�������Ľ�� & ����һ����ת����\",\"date\":\"2015-08-28 14:32\",\"view\":\"(121)\"},{\"title\":\"3.2 �绰�����Ӧ��Ӣ�ﵥ��\",\"date\":\"2015-08-27 18:24\",\"view\":\"(85)\"},{\"title\":\"07 java.lang.StringBuilder\",\"date\":\"2015-08-26 22:43\",\"view\":\"(119)\"},{\"title\":\"06 java.lang.AbstractStringBuilder\",\"date\":\"2015-08-26 22:20\",\"view\":\"(113)\"},{\"title\":\"3.1 �ַ�����λ��������\",\"date\":\"2015-08-26 21:51\",\"view\":\"(109)\"},{\"title\":\"2.19 �����غ��ж�\",\"date\":\"2015-08-26 21:30\",\"view\":\"(120)\"},{\"title\":\"05 java.lang.String\",\"date\":\"2015-08-25 22:39\",\"view\":\"(131)\"},{\"title\":\"2.18 ����ķָ�\",\"date\":\"2015-08-25 21:36\",\"view\":\"(99)\"},{\"title\":\"2.17 ����ѭ����λ\",\"date\":\"2015-08-25 20:31\",\"view\":\"(100)\"},{\"title\":\"04 java.lang.Byte\",\"date\":\"2015-08-24 22:44\",\"view\":\"(128)\"}]}],\"page-8\":[{\"products\":[{\"title\":\"2.16 ��һ�����е������������\",\"date\":\"2015-08-24 22:25\",\"view\":\"(140)\"},{\"title\":\"03 java.lang.Boolean\",\"date\":\"2015-08-20 21:13\",\"view\":\"(114)\"},{\"title\":\"2.15 ���ά������Ӿ��������\",\"date\":\"2015-08-20 20:58\",\"view\":\"(125)\"},{\"title\":\"2.14 ������֮�͵����ֵ\",\"date\":\"2015-08-20 20:03\",\"view\":\"(140)\"},{\"title\":\"02 java.lang.Integer\",\"date\":\"2015-08-19 21:39\",\"view\":\"(125)\"},{\"title\":\"2.13 ����������˻�\",\"date\":\"2015-08-19 21:05\",\"view\":\"(122)\"},{\"title\":\"2.12 �ҳ������к�Ϊ������ֵ����������\",\"date\":\"2015-08-19 20:31\",\"view\":\"(108)\"},{\"title\":\"00 ����\",\"date\":\"2015-08-18 21:17\",\"view\":\"(119)\"},{\"title\":\"01 java.lang.Object\",\"date\":\"2015-08-18 21:13\",\"view\":\"(118)\"},{\"title\":\"2.10 Ѱ�������е����ֵ����Сֵ\",\"date\":\"2015-08-18 20:39\",\"view\":\"(118)\"},{\"title\":\"2.9 Fibonacci����\",\"date\":\"2015-08-18 20:09\",\"view\":\"(102)\"},{\"title\":\"2.8 �ҳ�����n��ֻ��0, 1����\",\"date\":\"2015-08-17 22:17\",\"view\":\"(108)\"},{\"title\":\"2.8 Ѱ�ҷ�����������\",\"date\":\"2015-08-17 22:09\",\"view\":\"(109)\"},{\"title\":\"05 ħ��\",\"date\":\"2015-08-17 19:08\",\"view\":\"(150)\"},{\"title\":\"2.6/ 7 ��ȷ��ʾ������ + ���Լ��\",\"date\":\"2015-08-16 18:39\",\"view\":\"(114)\"}]}],\"page-9\":[{\"products\":[{\"title\":\"2.5 Ѱ������k����\",\"date\":\"2015-08-15 21:08\",\"view\":\"(112)\"},{\"title\":\"2.4 1����Ŀ\",\"date\":\"2015-08-12 21:25\",\"view\":\"(118)\"},{\"title\":\"04 �Թ�Ѱ·\",\"date\":\"2015-08-11 21:31\",\"view\":\"(154)\"},{\"title\":\"2.3 Ѱ��\\\"ˮ����\\\"\",\"date\":\"2015-08-11 20:38\",\"view\":\"(200)\"},{\"title\":\"2.2 �׳��е�����\",\"date\":\"2015-08-11 20:13\",\"view\":\"(112)\"},{\"title\":\"03 ������\",\"date\":\"2015-08-10 20:54\",\"view\":\"(136)\"},{\"title\":\"2.1 ��������1�ĸ���\",\"date\":\"2015-08-10 19:30\",\"view\":\"(126)\"},{\"title\":\"1.5 �����ҳ����ϵĻ���\",\"date\":\"2015-08-09 21:07\",\"view\":\"(147)\"},{\"title\":\"1.3 ���ӱ�����\",\"date\":\"2015-08-09 19:46\",\"view\":\"(193)\"},{\"title\":\"1.2 ��˧����\",\"date\":\"2015-08-09 16:29\",\"view\":\"(156)\"},{\"title\":\"02 ����˹����\",\"date\":\"2015-08-09 14:08\",\"view\":\"(159)\"},{\"title\":\"01 ̰����\",\"date\":\"2015-08-08 21:48\",\"view\":\"(178)\"},{\"title\":\"01 ��ƪ\",\"date\":\"2015-08-08 21:14\",\"view\":\"(147)\"},{\"title\":\"sessionFactory.openSession().save(Obj)��ִֻ����select max (id) from XXtable����\",\"date\":\"2014-08-06 18:16\",\"view\":\"(352)\"},{\"title\":\"hibernate, Bad format for DATE ��1�� in column 4������\",\"date\":\"2014-08-03 18:36\",\"view\":\"(1174)\"}]}],\"page-10\":[{\"products\":[{\"title\":\"��������������������ffH���ڵ����������ѡ����������������[���Ǻ��������ɣ�cpu̫����]\",\"date\":\"2014-07-27 15:44\",\"view\":\"(359)\"},{\"title\":\"��һ������Hello World\",\"date\":\"2014-07-27 15:39\",\"view\":\"(356)\"},{\"title\":\"�ڶ��������ж�����\",\"date\":\"2014-07-27 15:38\",\"view\":\"(353)\"}]}]}");
//		String pattern = "$this[0,3].title";
//		String pattern = "$this[0,3]";
//		String pattern = "$this[1?]";
//		String pattern = "$this['eval#1+1', eval#$len]";
//		String pattern = "$this[1?]";
//		JSONArray obj = JSONArray.fromObject("[{\"title\":\"08�����߳̽����ӡ121212...\",\"date\":\"2016-08-0921:43\",\"view\":\"(8)\"},{\"title\":\"17FileNameMatcher\",\"date\":\"2016-08-0721:20\",\"view\":\"(12)\"},{\"title\":\"16pointFixLike\",\"date\":\"2016-05-2122:00\",\"view\":\"(1841)\"},{\"title\":\"һ�����˵��۵���\",\"date\":\"2016-04-0923:08\",\"view\":\"(89)\"},{\"title\":\"15����Calender��صķ���\",\"date\":\"2016-03-0321:48\",\"view\":\"(103)\"},{\"title\":\"08scala,imported`Record'ispermanentlyhiddenbydefinitionofclassRecordinpackagetest\",\"date\":\"2016-03-0120:57\",\"view\":\"(161)\"},{\"title\":\"01SparkStreaming'sWordCount\",\"date\":\"2016-02-1221:49\",\"view\":\"(211)\"},{\"title\":\"14screenShotLikeQQ\",\"date\":\"2016-02-0220:53\",\"view\":\"(121)\"},{\"title\":\"13gifGenerator\",\"date\":\"2016-02-0120:36\",\"view\":\"(146)\"},{\"title\":\"12���ˮӡ\",\"date\":\"2016-01-2721:08\",\"view\":\"(100)\"},{\"title\":\"11����ѩ����̬ͼ\",\"date\":\"2016-01-2620:31\",\"view\":\"(102)\"},{\"title\":\"07�˻ʺ�����\",\"date\":\"2016-01-2620:08\",\"view\":\"(104)\"},{\"title\":\"10��������\",\"date\":\"2016-01-2521:05\",\"view\":\"(92)\"},{\"title\":\"30��n�����������ȡm������\",\"date\":\"2016-01-2420:44\",\"view\":\"(387)\"},{\"title\":\"29ͬλ�ʵ�ͳ��\",\"date\":\"2016-01-2320:33\",\"view\":\"(89)\"}]");
		
		
//		String pattern = "$this.pageSum";
//		String pattern = "$this.curTag";
//		String pattern = "$this.tagList[4].text";
//		String pattern = "$this.tagList[?].text";
//		String pattern = "$this.blogList[*].tags";
//		String pattern = "$this.blogList[*].tags[eval#$len/2]";
		String pattern = "$this.sdf|$this.blogList[*].tags[eval#$len/2]";
		
		JSONObject obj = JSONObject.fromObject("{\"tagList\":[{\"text\":\"all\",\"cnt\":8},{\"text\":\"java\",\"cnt\":7},{\"text\":\"lock\",\"cnt\":4},{\"text\":\"�ַ���ƥ��\",\"cnt\":3},{\"text\":\"cpp\",\"cnt\":1},{\"text\":\"eclipse\",\"cnt\":1},{\"text\":\"mingw\",\"cnt\":1}],\"blogList\":[{\"id\":16,\"title\":\"Boyer-Moore�㷨\",\"tags\":[\"�ַ���ƥ��\",\"java\"],\"date\":\"2016-01-3121:10\",\"good\":2,\"notGood\":1,\"visited\":41,\"commentsNum\":9},{\"id\":15,\"title\":\"KMP�㷨\",\"tags\":[\"�ַ���ƥ��\",\"java\"],\"date\":\"2016-01-3120:33\",\"good\":1,\"notGood\":1,\"visited\":9,\"commentsNum\":3},{\"id\":14,\"title\":\"�Զ����ַ���ƥ��\",\"tags\":[\"�ַ���ƥ��\",\"java\"],\"date\":\"2016-01-3120:12\",\"good\":0,\"notGood\":1,\"visited\":5,\"commentsNum\":1},{\"id\":13,\"title\":\"eclipseC&CPP���뺬�ж��main��������Ŀ\",\"tags\":[\"eclipse\",\"cpp\",\"mingw\"],\"date\":\"2015-12-2816:02\",\"good\":1,\"notGood\":0,\"visited\":6,\"commentsNum\":1},{\"id\":11,\"title\":\"Java���������Լ�����(��)��������\",\"tags\":[\"java\",\"lock\"],\"date\":\"2015-12-2521:59\",\"good\":0,\"notGood\":1,\"visited\":3,\"commentsNum\":0},{\"id\":10,\"title\":\"Java���������Լ�����(��)������\",\"tags\":[\"java\",\"lock\"],\"date\":\"2015-12-2521:58\",\"good\":1,\"notGood\":0,\"visited\":3,\"commentsNum\":0},{\"id\":9,\"title\":\"Java���������Լ�����(��)����������������\",\"tags\":[\"java\",\"lock\"],\"date\":\"2015-12-2521:58\",\"good\":0,\"notGood\":1,\"visited\":3,\"commentsNum\":1},{\"id\":8,\"title\":\"java���������Լ�����(һ)������\",\"tags\":[\"java\",\"lock\"],\"date\":\"2015-12-2521:56\",\"good\":0,\"notGood\":0,\"visited\":5,\"commentsNum\":0}],\"pageSum\":1,\"curTag\":\"all\"}");
		
		JSONArray res = JSONExtractor.extractInfoFromJSON(obj, pattern);
		info(res.toString() );
	}
	
}
