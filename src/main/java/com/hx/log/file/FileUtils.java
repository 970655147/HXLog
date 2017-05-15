/**
 * file name : FileUtils.java
 * created at : 22:02:05 2016-12-30
 * created by 970655147
 */

package com.hx.log.file;

import com.hx.common.interf.consumer.FileConsumer;
import com.hx.common.interf.consumer.StringConsumer;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.io.*;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import static com.hx.log.util.Log.err;
import static com.hx.log.util.Tools.assert0;

/**
 * �ļ�������صĹ���
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 11:41 PM
 */
public final class FileUtils {

    /**
     * Ĭ�ϵ�charset
     */
    public static String DEFAULT_CHARSET = Tools.DEFAULT_CHARSET;
    /**
     * �Ƿ��첽����д����
     */
    public static boolean WRITE_ASYNC = Tools.WRITE_ASYNC;

    // disable constructor
    private FileUtils() {
        assert0("can't instantiate !");
    }

    // ----------------- �ļ�������ط��� -----------------------

    /**
     * �ж��Ƿ���Ҫ��ӡ��־
     *
     * @param logFlags ��ǰ��log��־
     * @param logMask  ��������־��mask
     * @return
     */
    public static boolean isLog(long logFlags, long logMask) {
        return ((logFlags & logMask) != 0);
    }

    // ��content�ַ������浽ָ�����ļ���
    // add 'isAsync' at 2016.04.16

    /**
     * ���������ı����� ���浽�������ļ�
     *
     * @param content �������ı�����
     * @param file    ��Ҫ������ļ�
     * @param charset ������ַ���
     * @param isAsync �Ƿ��첽����
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:42 PM
     * @since 1.0
     */
    public static void save(String content, File file, String charset, boolean isAsync) throws IOException {
        write(content, file, charset, isAsync, false);
    }

    public static void save(String content, File file, String charset) throws IOException {
        save(content, file, charset, WRITE_ASYNC);
    }

    public static void save(String content, File file, boolean isAsync) throws IOException {
        save(content, file, DEFAULT_CHARSET, isAsync);
    }

    public static void save(String content, File file) throws IOException {
        save(content, file, DEFAULT_CHARSET, WRITE_ASYNC);
    }

    public static void save(String content, String file, String charset, boolean isAsync) throws IOException {
        save(content, new File(file), charset, isAsync);
    }

    public static void save(String content, String file, String charset) throws IOException {
        save(content, file, charset, WRITE_ASYNC);
    }

    public static void save(String content, String file, boolean isAsync) throws IOException {
        save(content, file, DEFAULT_CHARSET, isAsync);
    }

    public static void save(String content, String file) throws IOException {
        save(content, file, DEFAULT_CHARSET, WRITE_ASYNC);
    }

    /**
     * ���������ı����� ׷�ӵ��������ļ�
     *
     * @param content �������ı�����
     * @param file    ��Ҫ������ļ�
     * @param charset ������ַ���
     * @param isAsync �Ƿ��첽����
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:44 PM
     * @since 1.0
     */
    public static void append(String content, File file, String charset, boolean isAsync) throws IOException {
        write(content, file, charset, isAsync, true);
    }

    public static void append(String content, File file, String charset) throws IOException {
        append(content, file, charset, WRITE_ASYNC);
    }

    public static void append(String content, File file, boolean isAsync) throws IOException {
        append(content, file, DEFAULT_CHARSET, isAsync);
    }

    public static void append(String content, File file) throws IOException {
        append(content, file, DEFAULT_CHARSET, WRITE_ASYNC);
    }

    public static void append(String content, String file, String charset, boolean isAsync) throws IOException {
        append(content, new File(file), charset, isAsync);
    }

    public static void append(String content, String file, String charset) throws IOException {
        append(content, file, charset, WRITE_ASYNC);
    }

    public static void append(String content, String file, boolean isAsync) throws IOException {
        append(content, file, DEFAULT_CHARSET, isAsync);
    }

    public static void append(String content, String file) throws IOException {
        append(content, file, DEFAULT_CHARSET, WRITE_ASYNC);
    }

    // 1. could use 'tryWithResource' replace 'tryFinally'
    // 2. update 'BufferedOutputStream' with 'FileOutputStream' cause there need not 'Buffer'
    // at 2016.04.16

    /**
     * ������������ ����/��� ���������ļ�
     *
     * @param content  �������ı�����
     * @param file     ��Ҫ������ļ�
     * @param charset  ������ַ���
     * @param isAsync  �Ƿ��첽����
     * @param isAppend �Ƿ�׷��
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:45 PM
     * @since 1.0
     */
    public static void write(final String content, final File file, final String charset,
                             boolean isAsync, final boolean isAppend) throws IOException {
        assert0(content != null, "'content' can't be null ");
        assert0(file != null, "'file' can't be null ");
        assert0(charset != null, "'charset' can't be null ");

        Runnable writeTask = (new Runnable() {
            @Override
            public void run() {
                try (FileOutputStream fos = new FileOutputStream(file, isAppend)) {
                    fos.write(content.getBytes(charset));
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });

        if (!isAsync) {
            writeTask.run();
        } else {
            Tools.execute(writeTask);
        }
    }

    public static void write(final String content, final File file, final String charset,
                             final boolean isAppend) throws IOException {
        write(content, file, charset, WRITE_ASYNC, isAppend);
    }

    public static void write(final String content, final File file, final boolean isAppend) throws IOException {
        write(content, file, DEFAULT_CHARSET, WRITE_ASYNC, isAppend);
    }

    /**
     * �������ļ��Ƿ����
     *
     * @param file �������ļ�
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:46 PM
     * @since 1.0
     */
    public static boolean exists(File file) {
        assert0(file != null, "'file' can't be null ");
        return file.exists();
    }

    public static boolean exists(String path) {
        assert0(path != null, "'path' can't be null ");
        return exists(new File(path));
    }

    /**
     * �������������ļ�
     *
     * @param src ԭ�ļ�
     * @param dst Ŀ���ļ���
     * @return boolean
     * @author Jerry.X.He
     * @date 5/7/2017 2:35 PM
     * @since 1.0
     */
    public static boolean renameTo(File src, File dst) {
        if (!src.exists()) {
            Log.log("src file isn't exists !");
            return false;
        }
        if (dst.exists()) {
            Log.log("dst file is exists !");
            return false;
        }

        return src.renameTo(dst);
    }

    public static boolean renameTo(String src, String dst) {
        assert0(src != null, "'src' can't be null ");
        assert0(dst != null, "'dst' can't be null ");
        return renameTo(new File(src), new File(dst));
    }

    /**
     * �Ƴ�ָ�����ļ�
     *
     * @param file ��Ҫ�Ƴ����ļ�
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:46 PM
     * @since 1.0
     */
    public static void delete(File file) {
        assert0(file != null, "'path' can't be null ");

        if (file.exists()) {
            boolean isSucc = file.delete();
        }
    }

    public static void delete(String path) {
        assert0(path != null, "'path' can't be null ");
        delete(new File(path));
    }

    /**
     * ����ָ�����ļ�
     *
     * @param src Դ�ļ�
     * @param dst Ŀ���ļ�
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:47 PM
     * @since 1.0
     */
    public static void copy(File src, File dst) throws IOException {
        assert0(src != null, "'src' can't be null ");
        assert0(dst != null, "'dst' can't be null ");

        if (src.isDirectory()) {
            return;
        }
        if (!src.exists()) {
            return;
        }
        if (dst.exists()) {
            return;
        }

        FileInputStream fis = new FileInputStream(src);
        FileOutputStream fos = new FileOutputStream(dst);
        copy(fis, fos);
    }

    public static void copy(String src, String dst) throws IOException {
        assert0(src != null, "'src' can't be null ");
        assert0(dst != null, "'dst' can't be null ");
        copy(new File(src), new File(dst));
    }

    /**
     * ��ȡ�������������е��ַ�����
     *
     * @param is      �������ֽ���
     * @param charset �����ֽ���Ҫ�ı���
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:47 PM
     * @since 1.0
     */
    public static String getContent(InputStream is, String charset) throws IOException {
        assert0(is != null, "'inputStream' can't be null ");
        assert0(charset != null, "'charset' can't be null ");

        StringBuilder sb = new StringBuilder(is.available());
        try (BufferedReader br = new BufferedReader(new InputStreamReader(is, charset))) {
            String line = null;
            while ((line = br.readLine()) != null) {
                sb.append(line);
                sb.append(Tools.CRLF);
            }
        }

        return sb.toString();
    }

    public static String getContent(InputStream is) throws IOException {
        return getContent(is, DEFAULT_CHARSET);
    }

    public static String getContent(File file, String charset) throws IOException {
        return getContent(new FileInputStream(file), charset);
    }

    public static String getContent(File file) throws IOException {
        return getContent(file, DEFAULT_CHARSET);
    }

    public static String getContent(String path, String charset) throws IOException {
        return getContent(new File(path), charset);
    }

    public static String getContent(String path) throws IOException {
        return getContent(new File(path), DEFAULT_CHARSET);
    }

    /**
     * ��ȡ�ļ������е���, �洢��һ�������List, �ļ�����, ���ô˷���
     *
     * @param file         �������ļ�
     * @param charset      �����ֽ���Ҫ�ı���
     * @param estimateSize �����ļ�������
     * @return java.util.List<java.lang.String>
     * @author Jerry.X.He
     * @date 5/4/2017 11:48 PM
     * @since 1.0
     */
    public static List<String> getContentWithList(File file, String charset, int estimateSize) throws IOException {
        assert0(file != null, "'file' can't be null ");
        assert0(charset != null, "'charset' can't be null ");

        List<String> lines = new ArrayList<>(estimateSize);
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file), charset))) {
            String line = null;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        }

        return lines;
    }

    public static List<String> getContentWithList(File file, int estimateSize) throws IOException {
        return getContentWithList(file, DEFAULT_CHARSET, estimateSize);
    }

    public static List<String> getContentWithList(File file, String charset) throws IOException {
        return getContentWithList(file, charset, Tools.ESTIMATE_FILE_LINES);
    }

    public static List<String> getContentWithList(File file) throws IOException {
        return getContentWithList(file, DEFAULT_CHARSET);
    }

    public static List<String> getContentWithList(String file, String charset, int estimateSize) throws IOException {
        return getContentWithList(new File(file), charset, estimateSize);
    }

    public static List<String> getContentWithList(String file, int estimateSize) throws IOException {
        return getContentWithList(new File(file), estimateSize);
    }

    public static List<String> getContentWithList(String file, String charset) throws IOException {
        return getContentWithList(new File(file), charset);
    }

    public static List<String> getContentWithList(String file) throws IOException {
        return getContentWithList(new File(file));
    }

    /**
     * ʹ�ø���������������ָ�����ļ���ÿһ��
     *
     * @param file     �������ļ�
     * @param charset  �������ַ���
     * @param consumer ������������
     * @return java.util.List<java.lang.String>
     * @author Jerry.X.He
     * @date 5/12/2017 9:13 PM
     * @since 1.0
     */
    public static <T> T consumeContentList(File file, String charset, StringConsumer<T> consumer) throws IOException {
        assert0(file != null, "'file' can't be null ");
        assert0(charset != null, "'charset' can't be null ");
        assert0(consumer != null, "'consumer' can't be null ");

        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file), charset))) {
            String line = null;
            while ((line = br.readLine()) != null) {
                consumer.consume(line);
            }
        }
        return consumer.get();
    }

    public static <T> T consumeContentList(File file, StringConsumer<T> consumer) throws IOException {
        return consumeContentList(file, DEFAULT_CHARSET, consumer);
    }

    public static <T> T consumeContentList(String file, String charset, StringConsumer<T> consumer) throws IOException {
        return consumeContentList(new File(file), charset, consumer);
    }

    public static <T> T consumeContentList(String file, StringConsumer<T> consumer) throws IOException {
        return consumeContentList(new File(file), consumer);
    }

    /**
     * ��ָ����url��������ͼƬ  ���浽ָ����·������ [Ҳ���������������Ķ���������]
     *
     * @param urlStr ������url
     * @param path   ��Ҫ������ļ�·��
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:48 PM
     * @since 1.0
     */
    public static void downloadFrom(String urlStr, String path) throws IOException {
        assert0(urlStr != null, "'urlStr' can't be null ");
        assert0(path != null, "'path' can't be null ");

        URL url = new URL(urlStr);
        InputStream is = url.openStream();
        OutputStream os = new FileOutputStream(new File(path));
        copy(is, os);
    }

    /**
     * ���������е����� ���Ƶ������
     *
     * @param is            ������������
     * @param os            �����������
     * @param isCloseStream �Ƿ���Ҫ�ر���
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:49 PM
     * @since 1.0
     */
    public static void copy(InputStream is, OutputStream os, boolean isCloseStream) {
        assert0(is != null, "'inputStream' can't be null ");
        assert0(os != null, "'outputStream' can't be null ");

        BufferedInputStream bis = null;
        BufferedOutputStream bos = null;
        try {
            bis = new BufferedInputStream(is);
            bos = new BufferedOutputStream(os);
            int len = 0;
            byte[] buf = new byte[Tools.BUFF_SIZE_ON_TRANS_STREAM];
            while ((len = bis.read(buf)) != -1) {
                bos.write(buf, 0, len);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (isCloseStream) {
                if (bos != null) {
                    try {
                        bos.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
                if (bis != null) {
                    try {
                        bis.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }

    public static void copy(InputStream is, OutputStream os) {
        copy(is, os, true);
    }

    /**
     * ��ȡָ��·�����ļ����ļ�, ͨ��sep�ָ���ļ���     ��ȡ�ļ���
     * ����? ��λ��, ��Ϊ�˷�ֹһ�����
     *
     * @param path �������ļ�·��
     * @param sep  �ļ�·���ָ���
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:49 PM
     * @since 1.0
     */
    public static String getFileName(String path, char sep) {
        assert0(path != null, "'path' can't be null ");
        int start = path.lastIndexOf(sep) + 1;

//		http://webd.home.news.cn/1.gif?z=1&_wdxid=01002005057000300000000001110
        int end = getSymAfterFileName(path, start + 1);
        if (end != -1) {
            return path.substring(start, end);
        } else {
            return path.substring(start);
        }
    }

    /**
     * ���Ѹ������ļ�(��), �ݹ鴦��
     *
     * @param file     �������ļ�
     * @param consumer �������ļ�������
     * @return void
     * @author Jerry.X.He
     * @date 5/12/2017 10:20 PM
     * @since 1.0
     */
    public static <T> T traverseFiles(File file, FileConsumer<T> consumer) {
        Tools.assert0(file != null, "'file' can't be null !");
        Tools.assert0(consumer != null, "'consumer' can't be null !");

        if (!file.exists()) {
            err("please makeSure " + file.getPath() + " does exists ...");
            return consumer.get();
        }

        consumer.consume(file);
        if (file.isDirectory()) {
            File[] files = file.listFiles();
            if (!Tools.isEmpty(files)) {
                for (File childFile : files) {
                    traverseFiles(childFile, consumer);
                }
            }
        }

        return consumer.get();
    }

    public static <T> T traverseFiles(String folder, FileConsumer<T> consumer) {
        Tools.assert0(folder != null, "'folder' can't be null !");
        return traverseFiles(new File(folder), consumer);
    }


    // ----------------- �������� -----------------------

    /**
     * ��ȡ�ļ�������Ŀ��ܳ��ֵķ��ϵ����������
     *
     * @param path  ������·��
     * @param start ���һ���ļ��ָ�����λ��+1
     * @return int
     * @author Jerry.X.He
     * @date 5/4/2017 11:50 PM
     * @since 1.0
     */
    private static int getSymAfterFileName(String path, int start) {
        int min = -1;
        for (int i = start; i < path.length(); i++) {
            if (Tools.MAYBE_FILE_NAME_SEPS.contains(path.charAt(i))) {
                min = i;
                break;
            }
        }

        return min;
    }

}
